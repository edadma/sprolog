package ca.hyperreal.sprolog

import funl.lia.{FunctionMap, Math}


class PrologVM( evaluator: Evaluator = new Evaluator ) extends WAM
{
	def eval( a: Int ) = evaluator.eval( arg(a) )
	
	def identical =
	{
	val l = arg( 1 )
	val r = arg( 2 )
	
		if (l.isInstanceOf[NumberAST] && l.asInstanceOf[NumberAST].n.getClass != r.asInstanceOf[NumberAST].n.getClass)
			false
		else
			l == r
	}
	
	define( "is", 2 )
	{
	val v = eval(2)
	
		arg(1) match
		{
			case NumberAST( n, _ ) => n == v && n.getClass == v.getClass
			case a: Addr =>
				bind( a, ConCell(v) )
				true
			case _ => false
		}
	}
	
	define( "=:=", 2 ) (eval(1) == eval(2))
	
	define( "=\\=", 2 ) (eval(1) != eval(2))
	
	define( "<", 2 ) (Math( '<, eval(1), eval(2) ).asInstanceOf[Boolean])
	
	define( "=<", 2 ) (Math( '<=, eval(1), eval(2) ).asInstanceOf[Boolean])
	
	define( ">", 2 ) (Math( '>, eval(1), eval(2) ).asInstanceOf[Boolean])
	
	define( ">=", 2 ) (Math( '>=, eval(1), eval(2) ).asInstanceOf[Boolean])
	
	define( "==", 2 ) (identical)
	
	define( "\\==", 2 ) (!identical)
	
	define( "=", 2 ) (unify(addr(1), addr(2)))
	
	define( "\\=", 2 ) (!unify(addr(1), addr(2)))
		
	define( "arg", 3 )
	{
	val n = argInteger( 1 )
	val term = addr( 2 )
	
		if (unbound( term ))
			sys.error( "instantiation_error" )

		if (n < 0)
			sys.error( "domain_error" )

		if (isCompound( term ))
			unify( structureArg(term, n), addr(3) )
		else
			sys.error( "expected a structure" )
	}
	
	define( "atom", 1 ) (atom( arg(1) ))
	
	define( "atomic", 1 ) (atomic( arg(1) ))
	
	define( "call", 1 )
	{
	val start = callcode.size
	
		Prolog.compileQuery( arg(1), callcode )
		callcode(start) = CallAllocateInstruction( callcode(start).asInstanceOf[AllocateInstruction].n )
		cp = p
		p = start + QUERY
		true
	}
	
	define( "compound", 1 ) (compound( arg(1) ))
	
	define( "fail", 0 ) (false)
	
	define( "float", 1 )
	{
		arg(1) match
		{
			case NumberAST( (_: java.lang.Double|_: BigDecimal), _ ) => true
			case _ => false
		}
	}
	
	define( "functor", 3 )
	{
	val term = arg(1)
	val name = arg(2)
	val arity = arg(3)
	
		if (compound( term ))
		{
		val Indicator( _name, _arity ) = indicator( term )
		
			unify( setConstant(_name), addr(2) ) && unify( setConstant(_arity), addr(3) )
		}
		else if (atomic( term ))
			unify( setConstant(constant( term )), addr(2) ) && unify( setConstant(0), addr(3) )
		else if (variable( term ) && atomic( name ) && isInteger( arity ) && asInteger( arity ) == 0)
			unify( addr(1), addr(2) )
		else if (variable( term ) && atom( name ) && isInteger( arity ) && asInteger( arity ) > 0)
		{
		val _arity = asInteger( arity )
		val start = h
		
			put( h, FunCell(asSymbol(name), _arity) )
			h += 1
			
			for (_ <- 1 to _arity)
			{
				put( h, RefCell(h) )
				h += 1
			}
			
			unify( StrCell(start), addr(1) )
		}
		else
			false
	}
	
	define( "is_list", 1 )
	{
		isList( arg(1) )
	}
	
	define( "integer", 1 )
	{
		arg(1) match
		{
			case NumberAST( (_: java.lang.Integer|_: BigInt), _ ) => true
			case _ => false
		}
	}
	
	define( "nl", 0 )
	{
		println
		true
	}
	
	define( "nonvar", 1 ) (!unbound(addr(1)))
	
	define( "number", 1 ) (arg(1).isInstanceOf[NumberAST])
	
	define( "var", 1 ) (unbound(addr(1)))
	
	define( "true", 0 ) (true)
	
	define( "=..", 2 )
	{
	val terma = addr( 1 )
	
		if (unbound( terma ))
		{
		val list = argInstantiated( 2 )
	
			if (isList( list ))
				toList( list ) match
				{
					case List( c ) =>
						atomic( c ) && unify( terma, ConCell(constant(c)) )
					case f :: args =>
						if (atom( f ))
							unify( write(StructureAST(asAtom(f).atom, args.toIndexedSeq)), terma )
						else
							false
				}
			else
				sys.error( "expected list" )
		}
		else
		{
		val term = read( terma )
		val list = addr( 2 )
		
			if (atomic( term ))
				unify( write(fromList(List(term))), list )
			else
			{
			val s = term.asInstanceOf[StructureAST]
			
				unify( write(fromList(AtomAST(s.f) +: s.args.toList)), list )
			}
		}
	}
	
	define( "write", 1 )
	{
		print( display(arg(1)) )
		true
	}
	
	define( "iterator_", 2 )
	{
	val l = arg( 1 )
	
		isList( l ) && unify( setConstant(toList(l).iterator), addr(2) )
	}
	
	define( "next_", 2 )
	{
		unify( setConstant(constant(constant(arg(1)).asInstanceOf[Iterator[Any]].next.asInstanceOf[AST])), addr(2) )
	}
	
	define( "hasNext_", 1 )
	{
		constant(arg(1)).asInstanceOf[Iterator[Any]].hasNext
	}
}
