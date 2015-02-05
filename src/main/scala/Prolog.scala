package ca.hyperreal.sprolog

import java.io.{Reader, StringReader, ByteArrayOutputStream, PrintStream}

import collection.mutable.{ListBuffer, ArrayBuffer, HashMap, HashSet}

import ca.hyperreal.rtcep._


object Prolog
{
	val COMMA = Symbol( "," )
	val RULE = Symbol( ":-" )
	val DOT = Symbol( "." )
	val NIL = Symbol( "[]" )
	
	val parser =
		new AbstractPrologParser[AST]
		{
			def primary( value: Token ) =
				value.kind match
				{
 					case 'atom => AtomAST( Symbol(value.s), value.start.head.pos )
//					case 'atom => StructureAST( Symbol(value.s), IndexedSeq.empty, value.start.head.pos )
					case 'string => StringAST( value.s, value.start.head.pos )
					case 'integer => NumberAST( value.s.toInt, value.start.head.pos )
//					case 'variable if value.s == "_" => AnonymousAST( value.start.head.pos )
					case 'variable => VariableAST( Symbol(value.s), value.start.head.pos )
					case `nilsym` => StructureAST( nilsym, IndexedSeq.empty, value.start.head.pos )
					case _ => value.start.head.pos.error( "unrecognized token: [" + value.kind + "]" )
				}
			
			def structure( functor: Token, args: IndexedSeq[Value[AST]] ) =
				StructureAST(
					(functor.kind match
					{
						case 'atom|_: Character => Symbol(functor.s)
						case s: Symbol => s
					}),
					args.map(_.v), functor.start.head.pos )
		}
	
	def vm =
		new WAM
		{
			addCallable( "write", 1, _ => println( Prolog.display(read(1)) ) )
			addCallable( "fail", 0, _ => backtrack )
			addCallable( "=:=", 2, _ => if (read(1).asInstanceOf[NumberAST].n.intValue != read(2).asInstanceOf[NumberAST].n.intValue) backtrack )
			addCallable( "=\\=", 2, _ => if (read(1).asInstanceOf[NumberAST].n.intValue == read(2).asInstanceOf[NumberAST].n.intValue) backtrack )
			addCallable( "<", 2, _ => if (read(1).asInstanceOf[NumberAST].n.intValue >= read(2).asInstanceOf[NumberAST].n.intValue) backtrack )
			addCallable( "=<", 2, _ => if (read(1).asInstanceOf[NumberAST].n.intValue > read(2).asInstanceOf[NumberAST].n.intValue) backtrack )
			addCallable( ">", 2, _ => if (read(1).asInstanceOf[NumberAST].n.intValue <= read(2).asInstanceOf[NumberAST].n.intValue) backtrack )
			addCallable( ">=", 2, _ => if (read(1).asInstanceOf[NumberAST].n.intValue < read(2).asInstanceOf[NumberAST].n.intValue) backtrack )
			addCallable( "=", 2, _ => if (unify(addr(1), addr(2))) backtrack )
			addCallable( "\\=", 2, _ => if (!unify(addr(1), addr(2))) backtrack )
		}
		
	def parseClause( s: String ) = parser.parse( s, 4, '.' )
	
	def parseQuery( s: String ) =
	{
	val (query, rest) = parseClause( s )
	
		if (rest.head.end || rest.tail.head.end)
		{
			if (!query.isInstanceOf[StructureAST])
				query.pos.error( "expected a structure/atom" )
				
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
				c.pos.error( "expected a structure/atom" )
				
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
				case VariableAST( v, _ ) =>
					varmap.get(v) match
					{
						case None =>
							permmap.get(v) match
							{
								case None =>
									val res = Var( v, 0, r )
									
									varmap(v) = (0, r)
									r += 1
									res
								case Some( p ) =>
									val res = Var( v, 1, p )
									
									varmap(v) = (1, p)
									res
							}
						case Some( (b, n) ) =>
							Var( v, b, n )
					}
				case str: StructureAST =>
					val res = Var( null, 0, r )
					
					regmap(r) = str
					r += 1
					res
				case a => a
			}))
		
			regmap(reg) = s1
			
			for (i <- 0 until s1.arity)
				s1.args(i) match
				{
					case v: Var =>
						if (v.bank == 0 && regmap.contains( v.reg ))
							struct( v.reg )
					case _ =>
				}
		}
		
		struct( outerreg )
		r
	}
	
	def conjunctive( q: AST ): Stream[StructureAST] =
		q match
		{
			case StructureAST( COMMA, IndexedSeq(left: StructureAST, right: StructureAST), _ ) => left #:: conjunctive( right )
			case StructureAST( COMMA, IndexedSeq(_: StructureAST, right), _ ) => right.pos.error( "not a structure" )
			case StructureAST( COMMA, IndexedSeq(left, _), _ ) => left.pos.error( "not a structure" )
			case s: StructureAST => Stream( s )
			case _ => q.pos.error( "not a structure" )
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
				case VariableAST( v, _ ) => vars += v
				case StructureAST( _, args, _ ) =>
					for (a <- args)
						_structvars( a )
				case _ =>
			}
		}
		
		_structvars( q )
		vars.toList
	}
	
	def compileQuery( q: StructureAST ) =
	{
	val code = new ArrayBuffer[Instruction]
	val permvars = permanent( q, new HashSet[Symbol] )
	
		code += AllocateInstruction( permvars.size + 1 )
		new Query( body(q, code, permvars, new HashMap[Symbol, (Int, Int)], true) )
	}
	
	def body( q: StructureAST, code: ArrayBuffer[Instruction], permvars: Map[Symbol, Int], varmap: HashMap[Symbol, (Int, Int)], variables: Boolean ) =
	{
	val seen = new HashSet[(Int, Int)] ++ varmap.values
	
		for (t <- conjunctive( q ))
		{
		var nextreg = t.arity + 1
		
			for (arg <- 1 to t.arity)
			{
				t.args(arg - 1) match
				{
					case VariableAST( v, _ ) =>
						varmap.get( v ) match
						{
							case None =>
								permvars.get( v ) match
								{
									case None =>
										code += PutVariableInstruction( if (variables) v else null, 0, nextreg, arg )
										varmap(v) = (0, nextreg)
										seen add (0, nextreg)
										nextreg += 1
									case Some( n ) =>
										code += PutVariableInstruction( if (variables) v else null, 1, n, arg )
										varmap(v) = (1, n)
										seen add (1, n)
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
								s.args(i) match
								{
									case v: Var => 
										if (v.bank == 0 && regmap.contains( v.reg ))
											arrange( v.reg )
									case _ =>
								}
							}
							
							eqs += reg -> s
						}
						
						arrange( arg )
						
						for (e <- eqs)
						{
							code += PutStructureInstruction( FunCell(e._2.f, e._2.arity), e._1 )
							seen add (0, e._1)
							
							for (a <- e._2.args)
								a match
								{
									case Var( s, b, n ) =>
										if (seen( (b, n) ))
											code += SetValueInstruction( b, n )
										else
										{
											code += SetVariableInstruction( if (variables) s else null, b, n )
											seen add (b, n)
										}
									case AtomAST( atom, _ ) =>
										code += SetConstantInstruction( atom )
									case NumberAST( n, _ ) =>
										code += SetConstantInstruction( n )
								}
						}
					case AtomAST( atom, _ ) =>
						code += PutConstantInstruction( atom, arg )
					case NumberAST( n, _ ) =>
						code += PutConstantInstruction( n, arg )
				}
			}
			
			code += CallInstruction( Indicator(t.f, t.arity) )
			
			for ((k, v) <- varmap)
				if (v._1 == 0)
					varmap -= k
			
			for (e <- seen)
				if (e._1 == 0)
					seen -= e
		}

		code += DeallocateInstruction
		code.toVector
	}
	
	def fact( f: StructureAST, code: ArrayBuffer[Instruction], target: Indicator )
	{
	val start = code.length
	
		head( f, code, Map.empty )
		
		if (code.length > start && target != null)
			code(start).target( target )
		
		if (code.length == start && target != null)
			code += ProceedInstruction().target( target )
		else
			code += ProceedInstruction()
	}
	
	def rule( h: StructureAST, b: StructureAST, code: ArrayBuffer[Instruction], target: Indicator )
	{
	val permvars = permanent( b, new HashSet ++ structvars(h) )
	val pred = Indicator(h.f, h.arity)
	
		if (target eq null)
			code += AllocateInstruction( permvars.size + 1 )
		else
			code += AllocateInstruction( permvars.size + 1 ).target( pred )

		body( b, code, permvars, head(h, code, permvars), false )
	}
	
	def head( p: StructureAST, code: ArrayBuffer[Instruction], permvars: Map[Symbol, Int] ) =
	{
	val varmap = new HashMap[Symbol, (Int, Int)]
	var nextreg = p.arity + 1
	val regmap = new HashMap[Int, StructureAST]
	val seen = new HashSet[(Int, Int)]
	
		for (arg <- 1 to p.arity)
		{
			p.args(arg - 1) match
			{
				case VariableAST( v, _ ) =>
					varmap.get( v ) match
					{
						case None =>
							permvars.get( v ) match
							{
								case None =>
									code += GetVariableInstruction( 0, nextreg, arg )
									varmap(v) = (0, nextreg)
									seen add (0, nextreg)
									nextreg += 1
								case Some( n ) =>
									code += GetVariableInstruction( 1, n, arg )
									varmap(v) = (1, n)
									seen add (1, n)
							}
						case Some( (b, n) ) =>
							code += GetValueInstruction( b, n, arg )
					}
				case s: StructureAST =>
					regmap(arg) = s
				
					nextreg = struct( arg, nextreg, varmap, regmap, permvars )
					
					val e = regmap(arg)
					
					code += GetStructureInstruction( FunCell(e.f, e.arity), arg )
					seen add (0, arg)
					
					for (a <- e.args)
						a match
						{
							case Var( _, b, n ) =>
								if (seen( (b, n) ))
									code += UnifyValueInstruction( b, n )
								else
								{
									code += UnifyVariableInstruction( b, n )
									seen add (b, n)
								}
							case AtomAST( atom, _ ) =>
								code += UnifyConstantInstruction( atom )
						}
				case _ =>
			}
		}

		for (e <- regmap.toSeq.filter( a => a._1 > p.arity ).sortWith( (a, b) => a._1 < b._1 ))
		{
			code += GetStructureInstruction( FunCell(e._2.f, e._2.arity), e._1 )
			
			for (a <- e._2.args)
				a match
				{
					case Var( _, b, n ) =>
						if (seen( (b, n) ))
							code += UnifyValueInstruction( b, n )
						else
						{
							code += UnifyVariableInstruction( b, n )
							seen add (b, n)
						}
					case AtomAST( atom, _ ) =>
						code += UnifyConstantInstruction( atom )
				}
		}
		
		varmap
	}
	
	def clause( c: StructureAST, code: ArrayBuffer[Instruction], procmap: HashMap[Indicator, Int],
				proctype: HashMap[Indicator, Int], proclabel: HashMap[Indicator, Label] )
	{
	val pred =
		c match
		{
			case StructureAST( RULE, IndexedSeq(h: StructureAST, b: StructureAST), _ ) => Indicator( h.f, h.arity )
			case f: StructureAST => Indicator( f.f, f.arity )
		}
	var target: Indicator = null
	
		if (procmap contains pred)
		{
			proclabel( pred ) backpatch code.length
			
			if (proctype( pred ) > 1)
			{
			val l = new Label( code.length )
			
				code += RetryMeElseInstruction( l ).target( proclabel(pred) )
				proclabel( pred ) = l
				proctype( pred ) -= 1
			}
			else
				code += TrustMeInstruction().target( proclabel(pred) )
		}
		else
		{
			procmap( pred ) = code.length
			
			if (proctype( pred ) > 1)
			{
			val l = new Label( code.length )
			
				code += TryMeElseInstruction( l ).target( pred )
				proclabel( pred ) = l
				proctype( pred ) -= 1
			}
			else
				target = pred
		}
		
		c match
		{
			case StructureAST( RULE, IndexedSeq(h: StructureAST, b: StructureAST), _ ) => rule( h, b, code, target )
			case f: StructureAST => fact( f, code, target )
		}
	}
	
	def program( p: String ) = compileProgram( parseProgram(p) )
	
	def query( p: Program, q: String ) =
	{
	val buf = new StringBuilder
	val out = new ByteArrayOutputStream
	val v = vm
	
		v.program = p
		Console.withOut( new PrintStream(out, true) ) {v query compileQuery(parseQuery(q))}
		out.toString.trim
	}
	
	def queryFirst( p: Program, q: String ) =
	{
	val buf = new StringBuilder
	val out = new ByteArrayOutputStream
	val v = vm
	
		v.program = p
		Console.withOut( new PrintStream(out, true) ) {v queryFirst compileQuery(parseQuery(q))}
		out.toString.trim
	}
	
	def compileProgram( cs: List[StructureAST] ) =
	{
	val proctype = new HashMap[Indicator, Int]
	val proclabel = new HashMap[Indicator, Label]
	val procmap = new HashMap[Indicator, Int]
	
		for (c <- cs)
		{
		val pred =
			c match
			{
			case StructureAST( RULE, IndexedSeq(h: StructureAST, b: StructureAST), _ ) =>
				Indicator( h.f, h.arity )
			case f: StructureAST =>
				Indicator( f.f, f.arity )
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
	
	def listing( code: Seq[Instruction] )
	{
		for (i <- 0 until code.size)
		{
			print( code(i).label match 
				{
					case null => "\t"
					case l => l + ":\n\t"
				} )
			
			println( code(i) match
				{
				case PutStructureInstruction( f, i )		=> s"put_structure ${f.f.name}/${f.n}, $i"
				case SetVariableInstruction( v, b, i )	=> s"set_variable $v, $b, $i"
				case SetValueInstruction( b, i )			=> s"set_value $b, $i"
				case GetStructureInstruction( f, i )		=> s"get_structure ${f.f.name}/${f.n}, $i"
				case UnifyVariableInstruction( b, i )		=> s"unify_variable $b, $i"
				case UnifyValueInstruction( b, i )		=> s"unify_value $b, $i"
				case PutVariableInstruction( v, b, n, i )	=> s"put_variable $v, $b, $n, $i"
				case PutValueInstruction( b, n, i )		=> s"put_value $b, $n, $i"
				case GetVariableInstruction( b, n, i )	=> s"get_variable $b, $n, $i"
				case GetValueInstruction( b, n, i )		=> s"get_value $b, $n, $i"
				case CallInstruction( f )				=> s"call $f"
				case ProceedInstruction()				=> "proceed"
				case AllocateInstruction( n )				=> s"allocate ${n - 1}"
				case DeallocateInstruction				=> "deallocate"
				case TryMeElseInstruction( l )			=> s"try_me_else $l"
				case RetryMeElseInstruction( l )			=> s"retry_me_else $l"
				case TrustMeInstruction()				=> "trust_me"
				case PutConstantInstruction( c, i )		=> s"put_constant $c, $i"
				case GetConstantInstruction( c, i )		=> s"get_constant $c, $i"
				case SetConstantInstruction( c )			=> s"set_constant $c"
				case UnifyConstantInstruction( c )		=> s"unify_constant $c"
				} )
		}
	}
	
	case class Var( v: Symbol, bank: Int, reg: Int ) extends AST
	{
		val pos: Position = null
	}
	
	case class RHS( f: Symbol, args: Vector[Var] )
	case class Eq( lhs: Int, rhs: RHS )

	def isList( a: AST ): Boolean =
		a match
		{
			case StructureAST( NIL, IndexedSeq(), _ ) => true
			case StructureAST( DOT, IndexedSeq(head, tail), _ ) if isList( tail ) => true
			case _ => false
		}
		
	def toList( l: StructureAST ): List[AST] =
		l match
		{
			case StructureAST( NIL, IndexedSeq(), _ ) => Nil
			case StructureAST( DOT, IndexedSeq(head, tail: StructureAST), _ ) => head :: toList( tail )
		}
		
	def display( a: AST ): String =
		a match
		{
			case NumberAST( n, _ ) => n.toString
			case AtomAST( atom, _ ) => atom.name
			case VariableAST( s, _ ) => s.name
			case StructureAST( f, IndexedSeq(), _ ) => f.name
			case s: StructureAST if isList( s ) => toList( s ).map( display(_) ).mkString( "[", ", ", "]" )
			case StructureAST( f, args, _ ) => f.name + (for (a <- args) yield display( a )).mkString( "(", ", ", ")" )
		}
		
	def display( m: Map[String, AST] ): Map[String, String] = m map {case (k, v) => k -> display( v )}
}