package ca.hyperreal.sprolog

import java.io.{Reader, StringReader, ByteArrayOutputStream, PrintStream}

import collection.mutable.{ListBuffer, ArrayBuffer, HashMap, HashSet}

import ca.hyperreal.rtcep._

import funl.lia.{FunctionMap, Math}


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
					case 'intsym => NumberAST( value.s.toInt, value.start.head.pos )
 					case 'atom => AtomAST( Symbol(value.s), value.start.head.pos )
					case 'string => StringAST( value.s, value.start.head.pos )
					case 'integer => NumberAST( value.s.toInt, value.start.head.pos )
					case 'float => NumberAST( value.s.toDouble, value.start.head.pos )
					case 'variable if value.s == "_" => AnonymousAST( value.start.head.pos )
					case 'variable => VariableAST( Symbol(value.s), value.start.head.pos )
					case `nilsym` => AtomAST( nilsym, value.start.head.pos )
					case _ => value.start.head.pos.error( "unrecognized token: [" + value.kind + "]" )
				}
			
			def structure( functor: Token, args: IndexedSeq[Value[AST]] ) =
				if (functor.kind == '-' && args.length == 1 && args(0).v.isInstanceOf[NumberAST])
					args(0).v match
					{
						case NumberAST( n, _ ) => NumberAST( Math('-, n).asInstanceOf[Number], functor.start.head.pos )
					}
				else
					StructureAST(
						(functor.kind match
						{
							case 'atom|_: Character => Symbol(functor.s)
							case s: Symbol => s
						}),
						args.map(_.v), functor.start.head.pos )
		}
	
	val emptyProgram = new Program( Vector.empty[Instruction], Map.empty[Indicator, Int] )
		
	def parseClause( s: String ) = parser.parse( s, 4, '.' )
	
	def parseQuery( s: String ) =
	{
	val (query, rest) = parseClause( s )
	
		if (rest.head.end || rest.tail.head.end)	// . at the end of a query is optional
			query
		else
			rest.tail.head.pos.error( "unexpected input following query" )
	}
	
	def parseProgram( s: String ) =
	{
	val clauses = new ListBuffer[AST]
		
		def clause( toks: Stream[Token] )
		{
			if (!toks.head.end)
			{
			val (c, rest) = parser.parseTokens( toks, '.' )
			
				if (rest.head.kind != '.')
					rest.head.pos.error( "expected '.' following clause" )
					
// 				if (!c.isInstanceOf[StructureAST])
// 					c.pos.error( "expected a structure/atom" )
					
				clauses += c
				clause( rest.tail )
			}
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
	
	def conjunctive( q: AST ): Stream[AST] =
	{
		def transform( t: AST ) =
			t match
			{
				case v@VariableAST( _, pos ) => StructureAST( 'call, IndexedSeq(v), pos )
				case _ => t
			}
			
		q match
		{
			case StructureAST( COMMA, IndexedSeq(left, right), _ ) => transform( left ) #:: conjunctive( right )
// 			case StructureAST( COMMA, IndexedSeq(_: StructureAST, right), _ ) => right.pos.error( "not a structure" )
// 			case StructureAST( COMMA, IndexedSeq(left, _), _ ) => left.pos.error( "not a structure" )
			case _: StructureAST|_: AtomAST => Stream( q )
			case _: VariableAST => Stream( transform(q) )
			case _ => q.pos.error( "invalid query or rule body term" )
		}
	}
	
	def permanent( q: AST, varset: HashSet[Symbol] ) =
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
	
	def structvars( q: AST ) =
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
	
	def compileQuery( q: AST, code: ArrayBuffer[Instruction] = new ArrayBuffer[Instruction] ) =
	{
		if (q.isInstanceOf[VariableAST])
			sys.error( "instantiation error" )
			
	val permvars = permanent( q, new HashSet[Symbol] )
	
		code += AllocateInstruction( permvars.size + 1 )
		body( q, code, permvars, new HashMap[Symbol, (Int, Int)], true )
		code
	}
	
	def body( q: AST, code: ArrayBuffer[Instruction], permvars: Map[Symbol, Int], varmap: HashMap[Symbol, (Int, Int)], variables: Boolean )
	{
	val seen = new HashSet[(Int, Int)] ++ varmap.values
	val conj = conjunctive( q )
	val terms =
		if (conj.head.isInstanceOf[AtomAST] && conj.head.asInstanceOf[AtomAST].atom == '!)
		{
			code += NeckCutInstruction
			conj.tail
		}
		else
			conj
			
		for (term <- terms)
		{
			term match
			{
				case t: StructureAST =>
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
							case a: Addr =>
								code += PutRefInstruction( a, arg )
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
											case a: Addr =>
												code += SetRefInstruction( a )
											case AtomAST( atom, _ ) =>
												code += SetConstantInstruction( atom )
											case NumberAST( n, _ ) =>
												code += SetConstantInstruction( n )
											case StringAST( s, _ ) =>
												code += SetConstantInstruction( s )
											case AnonymousAST( _ ) =>
												code += SetVoidInstruction( 1 )
										}
								}
							case AtomAST( atom, _ ) =>
								code += PutConstantInstruction( atom, arg )
							case NumberAST( n, _ ) =>
								code += PutConstantInstruction( n, arg )
							case StringAST( s, _ ) =>
								code += PutConstantInstruction( s, arg )
							case AnonymousAST( _ ) =>
								code += PutVoidInstruction( arg )
						}
					}
					
					code += CallInstruction( Indicator(t.f, t.arity) )
				case AtomAST( a, _ ) =>
					code += CallInstruction( Indicator(a, 0) )
			}
			
			for ((k, v) <- varmap)
				if (v._1 == 0)
					varmap -= k
			
			for (e <- seen)
				if (e._1 == 0)
					seen -= e
		}

		code += DeallocateInstruction
	}
	
	def fact( f: AST, code: ArrayBuffer[Instruction], target: Indicator )
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
	
	def rule( h: AST, b: AST, code: ArrayBuffer[Instruction], target: Indicator )
	{
	val permvars = permanent( b, new HashSet ++ structvars(h) )
	val pred = indicator( h )
	
		if (target eq null)
			code += AllocateInstruction( permvars.size + 1 )
		else
			code += AllocateInstruction( permvars.size + 1 ).target( pred )

		body( b, code, permvars, head(h, code, permvars), false )
	}
	
	def head( h: AST, code: ArrayBuffer[Instruction], permvars: Map[Symbol, Int] ) =
	{
	val varmap = new HashMap[Symbol, (Int, Int)]
	
		h match
		{
			case p: StructureAST =>
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
									case NumberAST( n, _ ) =>
										code += UnifyConstantInstruction( n )
									case StringAST( s, _ ) =>
										code += UnifyConstantInstruction( s )
									case AnonymousAST( _ ) =>
										code += UnifyVoidInstruction( 1 )
								}
						case AtomAST( atom, _ ) =>
							code += GetConstantInstruction( atom, arg )
						case NumberAST( n, _ ) =>
							code += GetConstantInstruction( n, arg )
						case StringAST( s, _ ) =>
							code += GetConstantInstruction( s, arg )
						case AnonymousAST( _ ) =>
							// anonymous variable in argument position can be ignored - no code required
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
							case NumberAST( n, _ ) =>
								code += UnifyConstantInstruction( n )
							case StringAST( s, _ ) =>
								code += UnifyConstantInstruction( s )
							case AnonymousAST( _ ) =>
								code += UnifyVoidInstruction( 1 )
						}
				}
			case _ =>
		}
						
		varmap
	}
		
	def clause( c: AST, code: ArrayBuffer[Instruction], procmap: HashMap[Indicator, Int],
				proctype: HashMap[Indicator, Int], proclabel: HashMap[Indicator, Label] )
	{
	val pred =
		c match
		{
			case StructureAST( RULE, IndexedSeq(h, _), _ ) => indicator( h )
			case _ => indicator( c )
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
			case StructureAST( RULE, Seq(h, b), _ ) => rule( h, b, code, target )
			case f: StructureAST => fact( f, code, target )
			case a: AtomAST => fact( a, code, target )
		}
	}
	
	def program( p: String ) = compileProgram( parseProgram(p) )
	
	def query( p: Program, q: String ) =
	{
	val buf = new StringBuilder
	val out = new ByteArrayOutputStream
	val vm = new PrologVM
	
		vm.program = p
		Console.withOut( new PrintStream(out, true) ) {vm query compileQuery(parseQuery(q))}
		out.toString.trim
	}
	
	def queryOnce( p: Program, q: String ) =
	{
	val buf = new StringBuilder
	val out = new ByteArrayOutputStream
	val vm = new PrologVM
	
		vm.program = p
		Console.withOut( new PrintStream(out, true) ) {vm queryOnce compileQuery(parseQuery(q))}
		out.toString.trim
	}
	
	def compileProgram( cs: List[AST] ) =
	{
	val proctype = new HashMap[Indicator, Int]
	val proclabel = new HashMap[Indicator, Label]
	val procmap = new HashMap[Indicator, Int]
	
		for (c <- cs)
		{
		val pred =
			c match
			{
				case StructureAST( RULE, IndexedSeq(h, b), _ ) => indicator( h )
				case _: StructureAST | _: AtomAST => indicator( c )
				case _ => c.pos.error( "invalid fact or rule head" )
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
				case SetVoidInstruction( n )				=> s"set_void $n"
				case UnifyVoidInstruction( n )			=> s"unify_void $n"
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
			case AtomAST( NIL, _ ) => true
			case StructureAST( DOT, IndexedSeq(head, tail), _ ) if isList( tail ) => true
			case _ => false
		}
		
	def toList( l: AST ): List[AST] =
		l match
		{
			case AtomAST( NIL, _ ) => Nil
			case StructureAST( DOT, IndexedSeq(head, tail), _ ) => head :: toList( tail )
		}
		
	def display( a: AST ): String =
		a match
		{
			case NumberAST( n, _ ) => n.toString
			case AtomAST( atom, _ ) => atom.name
			case StringAST( s, _ ) => s
			case VariableAST( s, _ ) => s.name
			case _: Addr => a.toString
			case StructureAST( f, IndexedSeq(), _ ) => f.name
			case s: StructureAST if isList( s ) => toList( s ).map( display(_) ).mkString( "[", ", ", "]" )
			case StructureAST( f, args, _ ) => f.name + (for (a <- args) yield display( a )).mkString( "(", ", ", ")" )
		}
		
	def display( m: Map[String, AST] ): Map[String, String] = m map {case (k, v) => k -> display( v )}
}