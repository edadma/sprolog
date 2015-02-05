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
		
		t( a ).
		t( b ).
		t( c ).		
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
			"""	|A = a
				|A = b
				|A = c
				""".stripMargin.trim
	}
	
	"types" in
	{
	val p = program( """
		p( f(a) ).
		ia( 123 ).
		ina( f(123) ).
		sa( `asdf` ).
		sna( f(`asdf`) ).
		""" )
	
		query( p, "p( A )." ) shouldBe "A = f(a)"
		query( p, "ia( A )." ) shouldBe "A = 123"
		query( p, "ina( A )." ) shouldBe "A = f(123)"
		query( p, "A = `asdf`" ) shouldBe "A = asdf"
		query( p, "sa( A )." ) shouldBe "A = asdf"
		query( p, "sna( A )." ) shouldBe "A = f(asdf)"
	}
}