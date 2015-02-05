package ca.hyperreal.sprolog

import org.scalatest._
import prop.PropertyChecks

import Prolog.{program, query, queryFirst}


class Basic extends FreeSpec with PropertyChecks with Matchers
{
	"unification" in
	{
	val p = program( """
		p( f(X), h(Y, f(a)), Y ).
		
		q( Z, h(Z, W), f(W) ).
		
		r( a ).
		
		s( a ).
		s( b ).
		
		t( 1 ).
		t( 2 ).
		t( 3 ).
		
		u( f(1) ).
		
		v( g(a) ).
		""" )
	
		query( p, "A = a." ) shouldBe "A = a"
		query( p, "a = A." ) shouldBe "A = a"
		query( p, "A = f(a)." ) shouldBe "A = f(a)"
		query( p, "A = f(B), B = C, C = a." ) shouldBe "A = f(a), B = a, C = a"
		query( p, "p( Z, h(Z, W), f(W) )." ) shouldBe "W = f(a), Z = f(f(a))"
		query( p, "q( f(X), h(Y, f(a)), Y )." ) shouldBe "X = f(a), Y = f(f(a))"
		query( p, "r( a )." ) shouldBe "yes"
		query( p, "r( b )." ) shouldBe "no"
		query( p, "r( A )." ) shouldBe "A = a"
		query( p, "s( A )." ) shouldBe
			"""	|A = a
				|A = b
				""".stripMargin.trim
		query( p, "t( A )." ) shouldBe
			"""	|A = 1
				|A = 2
				|A = 3
				""".stripMargin.trim
		query( p, "u( A )." ) shouldBe "A = f(1)"
		query( p, "v( A )." ) shouldBe "A = g(a)"
	}
}