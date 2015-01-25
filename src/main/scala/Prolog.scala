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
	
	def struct( varmap: HashMap[Symbol, Int], regmap: HashMap[Int, StructureAST] )
	{
	var r = 2

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
		
		struct( 1 )
	}

	def query( q: StructureAST ) =
	{
	val varmap = new HashMap[Symbol, Int]
	val regmap = HashMap(1 -> q)
	
		struct( varmap, regmap )
		
	val eqs = new ArrayBuffer[(Int, StructureAST)]
	
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
		
		arrange( 1 )
		
	val code = new ArrayBuffer[Instruction]
	val seen = new HashSet[Int]
	
		for (e <- eqs)
		{
			code += PutStructureInstruction( e._2.f, e._2.arity, e._1 )
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
		
		(code.toVector, varmap.toMap)
	}
	
	def program( p: StructureAST ) =
	{
	val regmap = HashMap(1 -> p)
	
		struct( new HashMap[Symbol, Int], regmap )
		
	val code = new ArrayBuffer[Instruction]
	val seen = new HashSet[Int]
	
		for (e <- regmap.toSeq.sortWith( (a, b) => a._1 < b._1 ))
		{
			code += GetStructureInstruction( e._2.f, e._2.arity, e._1 )
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
		
		code.toVector
	}
}