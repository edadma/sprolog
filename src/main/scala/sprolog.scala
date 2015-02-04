package ca.hyperreal


package object sprolog
{
	case class Indicator( functor: Symbol, arity: Int )
	{
		override def toString = functor.name + "/" + arity
	}
}