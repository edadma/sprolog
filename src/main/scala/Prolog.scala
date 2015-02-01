package ca.hyperreal.sprolog

import java.io.{Reader, StringReader}

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
	val RULE = Symbol( ":-" )
	
	def parseClause( s: String ) = parser.parse( s, 4, '.' )
	
	def parseQuery( s: String ) =
	{
	val (query, rest) = parseClause( s )
	
		if (rest.head.end || rest.tail.head.end)
		{
			if (!query.isInstanceOf[StructureAST])
				sys.error( "expected a structure/atom" )
				
			query.asInstanceOf[StructureAST]
		}
		else
			rest.tail.head.pos.error( "unexpected input following query" )
	}
	
	def parseProgram( s: String ) =
	{
	val clauses = new ListBuffer[StructureAST]
		
		def clause( toks: Stream[Token] )
		{
		val (c, rest) = parser.parseTokens( toks, '.' )
		
			if (rest.head.kind != '.')
				rest.head.pos.error( "expected '.' following clause" )
				
			if (!c.isInstanceOf[StructureAST])
				sys.error( "expected a structure/atom" )
				
			clauses += c.asInstanceOf[StructureAST]
			
			if (!rest.tail.head.end)
				clause( rest.tail )
		}
		
		clause( parser.scan(new StringReader(s), 4) )
		clauses.toList
	}
	
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
	val ts = conjunctive( q )
	
		varset ++= structvars( ts.head )
		
		for (t <- ts.tail)
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
	val code = new ArrayBuffer[Instruction]
	val permvars = permanent( q, new HashSet[Symbol] )
	
		code += AllocateInstruction( permvars.size + 1 )
		new Query( body(q, code, permvars, new HashMap[Symbol, (Int, Int)], true) )
	}
	
	def body( q: StructureAST, code: ArrayBuffer[Instruction], permvars: Map[Symbol, Int], varmap: HashMap[Symbol, (Int, Int)], variables: Boolean ) =
	{
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
										code += PutVariableInstruction( if (variables) v else null, 0, nextreg, arg )
										varmap(v) = (0, nextreg)
										nextreg += 1
									case Some( n ) =>
										code += PutVariableInstruction( if (variables) v else null, 1, n, arg )
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
							
							for (Var( initial, v, b, n ) <- e._2.args.asInstanceOf[Seq[Var]])
								if (initial)
									code += SetVariableInstruction( if (variables) v else null, b, n )
								else
									code += SetValueInstruction( b, n )
						}
				}
			}
			
			code += CallInstruction( FunCell(t.f, t.arity) )
		}

		code += DeallocateInstruction
		code.toVector
	}
	
	def fact( f: StructureAST, code: ArrayBuffer[Instruction] )
	{
		head( f, code, Map.empty )
		code += ProceedInstruction
	}
	
	def rule( h: StructureAST, b: StructureAST, code: ArrayBuffer[Instruction] )
	{
	val permvars = permanent( b, new HashSet ++ structvars(h) )
	
		code += AllocateInstruction( permvars.size + 1 )
		body( b, code, permvars, head(h, code, permvars), false )
	}
	
	def head( p: StructureAST, code: ArrayBuffer[Instruction], permvars: Map[Symbol, Int] ) =
	{
	val varmap = new HashMap[Symbol, (Int, Int)]
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
		
		varmap
	}
	
	def clause( c: StructureAST, code: ArrayBuffer[Instruction], procmap: HashMap[FunCell, Int],
				proctype: HashMap[FunCell, Int], proclabel: HashMap[FunCell, Label] )
	{
	val pred =
		c match
		{
			case StructureAST( RULE, IndexedSeq(h: StructureAST, b: StructureAST) ) => FunCell( h.f, h.arity )
			case f: StructureAST => FunCell( f.f, f.arity )
		}
		
		if (procmap contains pred)
		{
			proclabel( pred ) backpatch code.length
			
			if (proctype( pred ) > 1)
			{
			val l = new Label
			
				code += RetryMeElseInstruction( l )
				proclabel( pred ) = l
				proctype( pred ) -= 1
			}
			else
				code += TrustMeInstruction
		}
		else
		{
			procmap( pred ) = code.length
			
			if (proctype( pred ) > 1)
			{
			val l = new Label
			
				code += TryMeElseInstruction( l )
				proclabel( pred ) = l
				proctype( pred ) -= 1
			}
		}
		
		c match
		{
			case StructureAST( RULE, IndexedSeq(h: StructureAST, b: StructureAST) ) => rule( h, b, code )
			case f: StructureAST => fact( f, code )
		}
	}
	
	def program( cs: List[StructureAST] ) =
	{
	val proctype = new HashMap[FunCell, Int]
	val proclabel = new HashMap[FunCell, Label]
	val procmap = new HashMap[FunCell, Int]
	
		for (c <- cs)
		{
		val pred =
			c match
			{
			case StructureAST( RULE, IndexedSeq(h: StructureAST, b: StructureAST) ) =>
				FunCell( h.f, h.arity )
			case f: StructureAST =>
				FunCell( f.f, f.arity )
			}
			
			if (proctype contains pred)
				proctype(pred) += 1
			else
				proctype(pred) = 1
		}
		
	val code = new ArrayBuffer[Instruction]
	val permvars = Map[Symbol, Int]()
	
		for (c <- cs)
			clause( c, code, procmap, proctype, proclabel )
		
		new Program( code.toVector, procmap.toMap )
	}
	
	case class Var( initial: Boolean, v: Symbol, bank: Int, reg: Int ) extends AST
	case class RHS( f: Symbol, args: Vector[Var] )
	case class Eq( lhs: Int, rhs: RHS )
}