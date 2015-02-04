package ca.hyperreal.sprolog

import org.scalatest._
import prop.PropertyChecks

import Prolog.{program, query, queryFirst}


class Basic extends FreeSpec with PropertyChecks with Matchers
{
	"unification" in
	{
	val p = program( """
		X = X.
		
		p( f(X), h(Y, f(a)), Y ).
		
		q( Z, h(Z, W), f(W) ).
		""" )
	
		query( p, "A = a." ) shouldBe "A = a"
		query( p, "a = A." ) shouldBe "A = a"
		query( p, "A = f(a)." ) shouldBe "A = f(a)"
		query( p, "A = f(B), B = C, C = a." ) shouldBe "A = f(a), B = a, C = a"
		query( p, "p( Z, h(Z, W), f(W) )." ) shouldBe "W = f(a), Z = f(f(a))"
		query( p, "q( f(X), h(Y, f(a)), Y )." ) shouldBe "X = f(a), Y = f(f(a))"
	}
}