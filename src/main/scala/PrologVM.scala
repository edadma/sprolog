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
	
		argvar(1) match
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
	
	define( "atom", 1 ) (arg(1).isInstanceOf[AtomAST])
	
	define( "atomic", 1 )
	{
		arg(1) match
		{
			case _: NumberAST | _: AtomAST | _: StringAST => true
			case _ => false
		}
	}
	
	define( "==", 2 ) (identical)
	
	define( "\\==", 2 ) (!identical)
	
	define( "=", 2 ) (unify(addr(1), addr(2)))
	
	define( "\\=", 2 ) (!unify(addr(1), addr(2)))
	
	define( "compound", 1 ) (arg(1).isInstanceOf[StructureAST])
	
	define( "fail", 0 ) (false)
	
	define( "nonvar", 1 ) (!unbound(addr(1)))
	
	define( "number", 1 ) (arg(1).isInstanceOf[NumberAST])
	
	define( "var", 1 ) (unbound(addr(1)))
	
	define( "true", 0 ) (true)
	
	define( "write", 1 )
		{
			println( Prolog.display(arg(1)) )
			true
		}
}
