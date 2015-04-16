package ca.hyperreal.sprolog

import collection.mutable.{HashMap, ArrayBuffer, Buffer, ArrayStack}

import ca.hyperreal.lia.{FunctionMap, Math}


class PredicateMap( evaluator: Evaluator ) extends HashMap[Indicator, Predicate]
{
	def eval( a: Int, w: WAMInterface ) = evaluator.eval( w.wam.arg(a) )
	
	def define( name: String, arity: Int )( c: Predicate )
	{
		addPredicate( name, arity, c )
	}
	
	def addPredicate( name: String, arity: Int, c: Predicate )
	{
	val ind = Indicator( Symbol(name), arity )
	
		if (this contains ind)
			sys.error( s"callable $ind already added" )
		else
			this(ind) = c
	}
}

class PrologBuiltins( evaluator: Evaluator = new Evaluator ) extends PredicateMap( evaluator )
{
	define( "traceon", 0 )
	{ w =>
		w.wam.trace = true
		true
	}
	
	define( "traceoff", 0 )
	{ w =>
		w.wam.trace = false
		true
	}
	
	def identical( w: WAMInterface ) =
	{
	val l = w.wam.arg( 1 )
	val r = w.wam.arg( 2 )
	
		if (l.isInstanceOf[NumberAST] && l.asInstanceOf[NumberAST].n.getClass != r.asInstanceOf[NumberAST].n.getClass)
			false
		else
			l == r
	}

	define( "is", 2 ) (w => w.unify( w.wam.addr(1), ConCell(eval(2, w)) ))
	
	define( "=:=", 2 ) (w => eval( 1, w ) == eval( 2, w ))
	
	define( "=\\=", 2 ) (w => eval( 1, w ) != eval( 2, w ))
	
	define( "<", 2 ) (w => Math( '<, eval(1, w), eval(2, w) ).asInstanceOf[Boolean])
	
	define( "=<", 2 ) (w => Math( '<=, eval(1, w), eval(2, w) ).asInstanceOf[Boolean])
	
	define( ">", 2 ) (w => Math( '>, eval(1, w), eval(2, w) ).asInstanceOf[Boolean])
	
	define( ">=", 2 ) (w => Math( '>=, eval(1, w), eval(2, w) ).asInstanceOf[Boolean])
	
	define( "==", 2 ) (identical)
	
	define( "\\==", 2 ) (!identical( _ ))
	
	define( "=", 2 ) (w => w.unify(w.wam.addr(1), w.wam.addr(2)))
	
	define( "\\=", 2 ) (w => !w.unify(w.wam.addr(1), w.wam.addr(2)))
		
	define( "arg", 3 )
	{ w =>
	val n = w.wam.argInteger( 1 )
	val term = w.wam.addr( 2 )
	
		if (w.wam.unbound( term ))
			sys.error( "instantiation_error" )

		if (n < 0)
			sys.error( "domain_error" )

		if (w.wam.isCompound( term ))
			w.unify( w.wam.structureArg(term, n), w.wam.addr(3) )
		else
			sys.error( "expected a structure" )
	}
	
	define( "atom", 1 ) (w => atom( w.wam.arg(1) ))
	
	define( "atomic", 1 ) (w => atomic( w.wam.arg(1) ))
	
	define( "call", 1 )
	{ w =>
	val start = w.wam.callcode.size
	
		Prolog.compileCall( w.wam.arg(1), w.wam.callcode )
		w.wam.cp = w.wam.p
		w.wam.p = start + w.wam.QUERY
		true
	}
	
	define( "compound", 1 ) (w => compound( w.wam.arg(1) ))
	
	define( "fail", 0 ) (_ => false)
	
	define( "float", 1 )
	{ w =>
		w.wam.arg(1) match
		{
			case NumberAST( (_: java.lang.Double|_: BigDecimal) ) => true
			case _ => false
		}
	}
	
	define( "functor", 3 )
	{ w =>
	val term = w.wam.arg(1)
	val name = w.wam.arg(2)
	val arity = w.wam.arg(3)
	
		if (compound( term ))
		{
		val Indicator( _name, _arity ) = indicator( term )
		
			w.unify( ConCell(_name), w.wam.addr(2) ) && w.unify( ConCell(_arity), w.wam.addr(3) )
		}
		else if (atomic( term ))
			w.unify( ConCell(constant( term )), w.wam.addr(2) ) && w.unify( ConCell(0), w.wam.addr(3) )
		else if (variable( term ) && atomic( name ) && w.wam.isInteger( arity ) && w.wam.asInteger( arity ) == 0)
			w.unify( w.wam.addr(1), w.wam.addr(2) )
		else if (variable( term ) && atom( name ) && w.wam.isInteger( arity ) && w.wam.asInteger( arity ) > 0)
		{
		val _arity = w.wam.asInteger( arity )
		val start = w.wam.h
		
			w.put( w.wam.h, FunCell(w.wam.asSymbol(name), _arity) )
			w.wam.h += 1
			
			for (_ <- 1 to _arity)
			{
				w.put( w.wam.h, RefCell(w.wam.h) )
				w.wam.h += 1
			}
			
			w.unify( StrCell(start), w.wam.addr(1) )
		}
		else
			false
	}
	
	define( "is_list", 1 )
	{ w =>
		isList( w.wam.arg(1) )
	}
	
	define( "integer", 1 )
	{ w =>
		w.wam.arg(1) match
		{
			case NumberAST( (_: java.lang.Integer|_: BigInt) ) => true
			case _ => false
		}
	}
	
	define( "nl", 0 )
	{ _ =>
		println
		true
	}
	
	define( "nonvar", 1 ) (w => !w.wam.unbound(w.wam.addr(1)))
	
	define( "number", 1 ) (w => w.wam.arg(1).isInstanceOf[NumberAST])
	
	define( "var", 1 ) (w => w.wam.unbound(w.wam.addr(1)))
	
	define( "true", 0 ) (_ => true)
	
	define( "=..", 2 )
	{ w =>
	val terma = w.wam.addr( 1 )
	
		if (w.wam.unbound( terma ))
		{
		val list = w.wam.argInstantiated( 2 )
	
			if (isList( list ))
				ca.hyperreal.sprolog.toList( list ) match
				{
					case List( c ) =>
						atomic( c ) && w.unify( terma, ConCell(constant(c)) )
					case f :: args =>
						if (atom( f ))
							w.unify( w.wam.write(StructureAST(asAtom(f).atom, args.toIndexedSeq)), terma )
						else
							false
				}
			else
				sys.error( "expected list" )
		}
		else
		{
		val term = w.wam.read( terma )
		val list = w.wam.addr( 2 )
		
			if (atomic( term ))
				w.unify( w.wam.write(fromList(List(term))), list )
			else
			{
			val s = term.asInstanceOf[StructureAST]
			
				w.unify( w.wam.write(fromList(AtomAST(s.f) +: s.args.toList)), list )
			}
		}
	}
	
	define( "write", 1 )
	{ w =>
		print( w.wam.display(w.wam.arg(1)) )
		true
	}
	
	define( "iterator_", 2 )
	{ w =>
	val l = w.wam.arg( 1 )
	
		isList( l ) && w.unify( ConCell(ca.hyperreal.sprolog.toList(l).iterator), w.wam.addr(2) )
	}
	
	define( "next_", 2 )
	{ w =>
		w.unify( ConCell(constant(constant(w.wam.arg(1)).asInstanceOf[Iterator[Any]].next.asInstanceOf[AST])), w.wam.addr(2) )
	}
	
	define( "hasNext_", 1 )
	{ w =>
		constant(w.wam.arg(1)).asInstanceOf[Iterator[Any]].hasNext
	}
}