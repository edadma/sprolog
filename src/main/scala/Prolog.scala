package ca.hyperreal.swam

import java.io.Reader

import collection.mutable.{ListBuffer, ArrayBuffer, HashMap, HashSet}

import ca.hyperreal.rtcep._


object Prolog
{
	val parser =
		new AbstractPrologParser[AST]
		{
			def primary( value: Token ) =
				value.kind match
				{
					case 'atom => StructureAST( Symbol(value.s), IndexedSeq.empty )
					case 'string => StringAST( value.s )
					case 'integer => NumberAST( value.s.toInt )
					case 'variable => VariableAST( Symbol(value.s) )
					case _ => value.start.head.pos.error( "unrecognized token: [" + value.kind + "]" )
				}
			
			def structure( functor: Token, args: IndexedSeq[Value[AST]] ) =
				StructureAST(
					(functor.kind match
					{
						case 'atom|_: Character => Symbol(functor.s)
						case s: Symbol => s
					}),
					args.map(_.v) )
		}
	
	val COMMA = Symbol( "," )
	
	def parse( s: String ) = parser.parse( s, 4, '.' )
	
	def struct( outerreg: Int, nextreg: Int, varmap: HashMap[Symbol, (Int, Int)], regmap: HashMap[Int, StructureAST], permmap: Map[Symbol, Int] ) =
	{
	var r = nextreg

		def struct( reg: Int )
		{
		val s = regmap(reg)
		val s1 = StructureAST( s.f, s.args.map (
			_ match
			{
				case VariableAST( v ) =>
					varmap.get(v) match
					{
						case None =>
							permmap.get(v) match
							{
								case None =>
									val res = Var( true, v, 0, r )
									
									varmap(v) = (0, r)
									r += 1
									res
								case Some( p ) =>
									val res = Var( true, v, 1, p )
									
									varmap(v) = (1, p)
									res
							}
						case Some( (b, n) ) =>
							Var( false, v, b, n )
					}
				case str: StructureAST =>
					val res = Var( true, null, 0, r )
					
					regmap(r) = str
					r += 1
					res
			}))
		
			regmap(reg) = s1
			
			for (i <- 0 until s1.arity)
			{
			val v = s1.args(i).asInstanceOf[Var]
			
				if (v.bank == 0 && regmap.contains( v.reg ))
					struct( v.reg )
			}
		}
		
		struct( outerreg )
		r
	}
	
	def conjunctive( q: StructureAST ): Stream[StructureAST] =
		q match
		{
			case StructureAST( COMMA, IndexedSeq(left: StructureAST, right: StructureAST ) ) => left #:: conjunctive( right )
			case StructureAST( COMMA, IndexedSeq(left, right: StructureAST ) ) => sys.error( "left argument not a structure" )
			case StructureAST( COMMA, IndexedSeq(left: StructureAST, right ) ) => sys.error( "right argument not a structure" )
			case _: StructureAST => Stream( q )
			case _ => sys.error( "not a structure" )
		}
	
	def permanent( q: StructureAST, varset: HashSet[Symbol] ) =
	{
	val permset = HashSet[Symbol]()
	
		for (t <- conjunctive( q ))
		{
		val vars = structvars( t ).toSet
		
			permset ++= varset intersect vars
			varset ++= vars
		}
		
	val permvars = new HashMap[Symbol, Int]
	var p = 1
	
		for (t <- conjunctive( q ); s <- structvars( t ) if permset( s ))
		{
			permvars(s) = p
			permset -= s
			p += 1
		}

		permvars.toMap
	}
	
	def structvars( q: StructureAST ) =
	{
	val vars = new ListBuffer[Symbol]
	
		def _structvars( t: AST )
		{
			t match
			{
				case VariableAST( v ) => vars += v
				case StructureAST( _, args ) =>
					for (a <- args)
						_structvars( a )
			}
		}
		
		_structvars( q )
		vars.toList
	}
	
	def query( q: StructureAST ) =
	{
	val permvars = permanent( q, new HashSet[Symbol] )
	val varmap = new HashMap[Symbol, (Int, Int)]
	val code = new ArrayBuffer[Instruction]

		code += AllocateInstruction( permvars.size + 1 )
		
		for (t <- conjunctive( q ))
		{
		var nextreg = t.arity + 1
		
			for (arg <- 1 to t.arity)
			{
				t.args(arg - 1) match
				{
					case VariableAST( v ) =>
						varmap.get( v ) match
						{
							case None =>
								permvars.get( v ) match
								{
									case None =>
										code += PutVariableInstruction( v, 0, nextreg, arg )
										varmap(v) = (0, nextreg)
										nextreg += 1
									case Some( n ) =>
										code += PutVariableInstruction( v, 1, n, arg )
										varmap(v) = (1, n)
								}
							case Some( (b, n) ) =>
								code += PutValueInstruction( b, n, arg )
						}
					case s: StructureAST =>
						val regmap = HashMap(arg -> s)
						val eqs = new ArrayBuffer[(Int, StructureAST)]
						
						nextreg = struct( arg, nextreg, varmap, regmap, permvars )
						
						def arrange( reg: Int )
						{
						val s = regmap(reg)
						
							for (i <- 0 until s.arity)
							{
							val v = s.args(i).asInstanceOf[Var]
							
								if (v.bank == 0 && regmap.contains( v.reg ))
									arrange( v.reg )
							}
							
							eqs += reg -> s
						}
						
						arrange( arg )
						
						for (e <- eqs)
						{
							code += PutStructureInstruction( FunCell(e._2.f, e._2.arity), e._1 )
							
							for (Var( initial, _, b, n ) <- e._2.args.asInstanceOf[Seq[Var]])
								if (initial)
									code += SetVariableInstruction( null, b, n )
								else
									code += SetValueInstruction( b, n )
						}
				}
			}
			
			code += CallInstruction( FunCell(t.f, t.arity) )
		}

		code += DeallocateInstruction
		new Query( code.toVector )
	}
	
	def program( p: StructureAST ) =
	{
	val permvars = Map[Symbol, Int]()
	val varmap = new HashMap[Symbol, (Int, Int)]
	val code = new ArrayBuffer[Instruction]
	var nextreg = p.arity + 1
	val regmap = new HashMap[Int, StructureAST]
	
		for (arg <- 1 to p.arity)
		{
			p.args(arg - 1) match
			{
				case VariableAST( v ) =>
					varmap.get( v ) match
					{
						case None =>
							permvars.get( v ) match
							{
								case None =>
									code += GetVariableInstruction( 0, nextreg, arg )
									varmap(v) = (0, nextreg)
									nextreg += 1
								case Some( n ) =>
									code += GetVariableInstruction( 1, n, arg )
									varmap(v) = (1, n)
							}
						case Some( (b, n) ) =>
							code += GetValueInstruction( b, n, arg )
					}
				case s: StructureAST =>
					regmap(arg) = s
				
					nextreg = struct( arg, nextreg, varmap, regmap, permvars )
					
					val e = regmap(arg)
					
					code += GetStructureInstruction( FunCell(e.f, e.arity), arg )
					
					for (Var( initial, _, b, n ) <- e.args.asInstanceOf[Seq[Var]])
						if (initial)
							code += UnifyVariableInstruction( b, n )
						else
							code += UnifyValueInstruction( b, n )
			}
		}

		for (e <- regmap.toSeq.filter( a => a._1 > p.arity ).sortWith( (a, b) => a._1 < b._1 ))
		{
			code += GetStructureInstruction( FunCell(e._2.f, e._2.arity), e._1 )
			
			for (Var( initial, _, b, n ) <- e._2.args.asInstanceOf[Seq[Var]])
				if (initial)
					code += UnifyVariableInstruction( b, n )
				else
					code += UnifyValueInstruction( b, n )
		}

		code += ProceedInstruction
		new Program( code.toVector, Map(FunCell(p.f, p.arity) -> 0) )
	}
	
	case class Var( initial: Boolean, v: Symbol, bank: Int, reg: Int ) extends AST
	case class RHS( f: Symbol, args: Vector[Var] )
	case class Eq( lhs: Int, rhs: RHS )
}