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
	
	def unbound( a: Address ) =
		a.read match
		{
			case PtrCell('ref, ptr ) if ptr == a => true
			case _ => false
		}
	
	def deref( store: Store, a: Int ): Address = deref( new Addr(store, a) )
	
	def deref( a: Address ): Address =
		a.read match
		{
			case PtrCell( 'ref, v ) if v != a => deref( v )
			case _ => a
		}
	
	def read( a: Address ): AST =
	{
		deref( a ).read match
		{
			case PtrCell( 'ref, a: Addr ) => a
			case PtrCell( 'str, p: Addr ) =>
				val FunCell( f, n ) = p.read
				
				StructureAST( f, for (i <- 1 to n) yield read( p + i ) )
			case ConCell( c ) =>
				c match
				{
					case s: Symbol => AtomAST( s )
					case n: Number => NumberAST( n )
					case s: String => StringAST( s )
				}
		}
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