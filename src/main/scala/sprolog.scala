package ca.hyperreal


package object sprolog
{
	case class Indicator( functor: Symbol, arity: Int )
	{
		override def toString = functor.name + "/" + arity
	}
	
	def indicator( t: AST ) =
		t match
		{
			case s: StructureAST => Indicator( s.f, s.arity )
			case AtomAST( a, _ ) => Indicator( a, 0 )
		}
	
	def atom( t: AST ) = t.isInstanceOf[AtomAST]
	
	def atomic( t: AST ) =
		t match
		{
			case _: NumberAST | _: AtomAST | _: StringAST => true
			case _ => false
		}
	
	def constant( t: AST ): Any =
		t match
		{
			case NumberAST( n, _ ) => n
			case AtomAST( a, _ ) => a
			case StringAST( s, _ ) => s
			case _ => sys.error( t + " not a non-compound constant" )
		}
	
	def compound( t: AST ) = t.isInstanceOf[StructureAST]
	
	def variable( t: AST ) = t.isInstanceOf[Addr]
}