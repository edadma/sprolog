package xyz.hyperreal


package object sprolog
{
	val DOT = Symbol( "." )
	val NIL = Symbol( "[]" )
	
	type Predicate = WAMInterface => Boolean
	
	case class Indicator( functor: Symbol, arity: Int ) extends Ordered[Indicator]
	{
		def compare( that: Indicator ) =
			if (functor.name < that.functor.name)
				-1
			else if (functor.name > that.functor.name)
				1
			else
				arity - that.arity
				
		override def toString = functor.name + "/" + arity
	}
	
	def indicator( t: AST ) =
		t match
		{
			case s: StructureAST => Indicator( s.f, s.arity )
			case AtomAST( a ) => Indicator( a, 0 )
		}
	
	def atom( t: AST ) = t.isInstanceOf[AtomAST]
	
	def asAtom( t: AST ) = t.asInstanceOf[AtomAST]
	
	def atomic( t: AST ) =
		t match
		{
			case _: NumberAST | _: AtomAST | _: StringAST | _: ConstantAST => true
			case _ => false
		}
	
	def constant( t: AST ): Any =
		t match
		{
			case NumberAST( n ) => n
			case AtomAST( a ) => a
			case StringAST( s ) => s
			case ConstantAST( c ) => c
			case _ => sys.error( t + " not a non-compound constant" )
		}
	
	def compound( t: AST ) = t.isInstanceOf[StructureAST]
	
	def variable( t: AST ) = t.isInstanceOf[Addr]

	def isList( a: AST ): Boolean =
		a match
		{
			case AtomAST( NIL ) => true
			case StructureAST( DOT, Seq(head, tail) ) if isList( tail ) => true
			case _ => false
		}
		
	def toList( l: AST ): List[AST] =
		l match
		{
			case AtomAST( NIL ) => Nil
			case StructureAST( DOT, Seq(head, tail) ) => head :: toList( tail )
		}

	def isVarList( a: AST ): Boolean =
		a match
		{
			case _: Addr => true
			case StructureAST( DOT, Seq(head, tail) ) if isVarList( tail ) => true
			case _ => false
		}
		
	def toVarList( l: AST ): List[AST] =
		l match
		{
			case a: Addr => List( a )
			case StructureAST( DOT, Seq(head, tail) ) => head :: toVarList( tail )
		}
	
	def fromList( l: List[AST] ): AST =
		l match
		{
			case Nil => AtomAST( NIL )
			case head :: tail =>
				StructureAST( DOT, IndexedSeq(head, fromList(tail)) )
		}
}