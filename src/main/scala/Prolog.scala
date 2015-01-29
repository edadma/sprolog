package ca.hyperreal.swam

import java.io.Reader

import collection.mutable.{ArrayBuffer, HashMap, HashSet}

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
	
	def parse( s: String ) = parser.parse( s, 4, '.' )

	val vars = new ArrayBuffer[(String, Int)]
	
	def struct( outerreg: Int, nextreg: Int, varmap: HashMap[Symbol, Int], regmap: HashMap[Int, StructureAST] ) =
	{
	var r = nextreg

		def struct( reg: Int )
		{
		val s = regmap(reg).asInstanceOf[StructureAST]
		val s1 = StructureAST( s.f, s.args.map (
			_ match
			{
				case v@VariableAST( n ) =>
					varmap.get(n) match
					{
						case None =>
							val res = IntAST( r )
							
							varmap(n) = r
							vars += (n.name + r -> r)
							r += 1
							res
						case Some( ind ) =>
							IntAST( ind )
					}
				case str: StructureAST =>
					val res = IntAST( r )
					
					regmap(r) = str
					r += 1
					res
			}))
		
			regmap(reg) = s1
			
			for (i <- 0 until s1.arity)
			{
			val n = s1.args(i).asInstanceOf[IntAST].n
			
				if (regmap contains n)
					struct( n )
			}
		}
		
		struct( outerreg )
		r
	}
	
	def query( q: StructureAST ) =
	{
	val varmap = new HashMap[Symbol, Int]
	var nextreg = q.arity + 1
	val code = new ArrayBuffer[Instruction]
	val seen = new HashSet[Int]
	
		for (arg <- 1 to q.arity)
		{
			q.args(arg - 1) match
			{
				case VariableAST( v ) =>
					varmap.get( v ) match
					{
						case None =>
							code += PutVariableInstruction( nextreg, arg )
							varmap(v) = nextreg
							seen += nextreg
							nextreg += 1
						case Some( n ) =>
							code += PutValueInstruction( n, arg )
					}
				case s: StructureAST =>
					val regmap = HashMap(arg -> s)
					val eqs = new ArrayBuffer[(Int, StructureAST)]
					
					nextreg = struct( arg, nextreg, varmap, regmap )
					
					def arrange( reg: Int )
					{
					val s = regmap(reg).asInstanceOf[StructureAST]
					
						for (i <- 0 until s.arity)
						{
						val r = s.args(i).asInstanceOf[IntAST].n
						
							if (regmap contains r)
								arrange( r )
						}
						
						eqs += reg -> s
					}
					
					arrange( arg )
					
					for (e <- eqs)
					{
						code += PutStructureInstruction( FunCell(e._2.f, e._2.arity), e._1 )
						seen += e._1
						
						for (IntAST( n ) <- e._2.args.asInstanceOf[Seq[IntAST]])
							if (seen(n))
								code += SetValueInstruction( n )
							else
							{
								code += SetVariableInstruction( n )
								seen += n
							}
					}
			}
		}
		
		code += CallInstruction( FunCell(q.f, q.arity) )
		new Query( code.toVector, varmap.toMap.map(_.swap) )
	}
	
	def program( p: StructureAST ) =
	{
	val varmap = new HashMap[Symbol, Int]
	val code = new ArrayBuffer[Instruction]
	val seen = new HashSet[Int]
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
							code += GetVariableInstruction( nextreg, arg )
							varmap(v) = nextreg
							seen += nextreg
							nextreg += 1
						case Some( n ) =>
							code += GetValueInstruction( n, arg )
					}
				case s: StructureAST =>
					regmap(arg) = s
				
					nextreg = struct( arg, nextreg, varmap, regmap )
					
					val e = regmap(arg)
					
					code += GetStructureInstruction( FunCell(e.f, e.arity), arg )
					seen += arg
					
					for (IntAST( n ) <- e.args.asInstanceOf[Seq[IntAST]])
						if (seen(n))
							code += UnifyValueInstruction( n )
						else
						{
							code += UnifyVariableInstruction( n )
							seen += n
						}
			}
		}

		for (e <- regmap.toSeq.filter( a => a._1 > p.arity ).sortWith( (a, b) => a._1 < b._1 ))
		{
			code += GetStructureInstruction( FunCell(e._2.f, e._2.arity), e._1 )
			seen += e._1
			
			for (IntAST( n ) <- e._2.args.asInstanceOf[Seq[IntAST]])
				if (seen(n))
					code += UnifyValueInstruction( n )
				else
				{
					code += UnifyVariableInstruction( n )
					seen += n
				}
		}

		code += ProceedInstruction
		new Program( code.toVector, Map(FunCell(p.f, p.arity) -> 0) )
	}
}