package xyz.hyperreal.sprolog

import org.scalatest._
import prop.PropertyChecks

import Prolog.{program, query, queryOnce}


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
		
		u( _, a, _ ).
		
		v( a, b ).
		""" )
	
		query( p, "A = a." ) shouldBe "A = a"
		query( p, "a = A." ) shouldBe "A = a"
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
		query( p, "u( asdf, A, zxcv )." ) shouldBe "A = a"
		query( p, "v( A, _ )." ) shouldBe "A = a"
	}
	
	"types" in
	{
	val p = program( """
		ia( 123 ).
		ina( f(123) ).
		sa( `asdf` ).
		sna( f(`asdf`) ).
		""" )
	
		query( p, "A = f(a)." ) shouldBe "A = f(a)"
		query( p, "A = 123" ) shouldBe "A = 123"
		query( p, "A = f(123)" ) shouldBe "A = f(123)"
		query( p, "ia( A )." ) shouldBe "A = 123"
		query( p, "ina( A )." ) shouldBe "A = f(123)"
		query( p, "A = `asdf`" ) shouldBe "A = asdf"
		query( p, "A = f(`asdf`)" ) shouldBe "A = f(asdf)"
		query( p, "sa( A )." ) shouldBe "A = asdf"
		query( p, "sna( A )." ) shouldBe "A = f(asdf)"
	}
}