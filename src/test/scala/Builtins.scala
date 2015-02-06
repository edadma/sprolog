package ca.hyperreal.sprolog

import org.scalatest._
import prop.PropertyChecks

import Prolog.{program, query, queryOnce}


class Builtins extends FreeSpec with PropertyChecks with Matchers
{
	"type testing" in
	{
	val p = program( """
		""" )
		// arithmetic compare 40
		query( p, "X = 1+2, X + 6 =:= X * 3." ) shouldBe "X = +(1, 2)"
		query( p, "'=:='(1.0, 1)." ) shouldBe "yes"
		query( p, "0.3333333333333333 =:= 1/3." ) shouldBe "no"

		// atom 50
		query( p, "atom('Yety')." ) shouldBe "yes"
		query( p, "atom([])." ) shouldBe "yes"
		query( p, "atom(f(X))." ) shouldBe "no"
		query( p, "atom(10.01)." ) shouldBe "no"
	
		// atomic 57
		query( p, "atomic(10.01)." ) shouldBe "yes"
		query( p, "atomic('Yeti')." ) shouldBe "yes"
//		query( p, "atomic((;))." ) shouldBe "yes"
		query( p, "atomic(X)." ) shouldBe "no"
		query( p, "atomic(f(X,Y))." ) shouldBe "no"

		// compound 72
		query( p, "compound(f(X,Y))." ) should not be "no"
		query( p, "compound([a])." ) shouldBe "yes"
		query( p, "compound(-a)." ) shouldBe "yes"
		query( p, "compound(-1)." ) shouldBe "no"
		query( p, "compound(10.01)." ) shouldBe "no"
		query( p, "compound('ok')." ) shouldBe "no"
		query( p, "compound([])." ) shouldBe "no"
		query( p, "compound(A)." ) shouldBe "no"

		// is 111
		query( p, "X = 1+2, Y is X*3." ) shouldBe "X = +(1, 2), Y = 9"
		query( p, "Result is 3+11.0." ) shouldBe "Result = 14.0"
		query( p, "1 is 1.0." ) shouldBe "no"
		
		// number 117
		query( p, "number(10.01)." ) shouldBe "yes"
		query( p, "number(-10)." ) shouldBe "yes"
		query( p, "number('ok')." ) shouldBe "no"
		query( p, "number(X)." ) shouldBe "no"
		query( p, "number(f(X, Y))." ) shouldBe "no"
		
		// var 181
		query( p, "var(X)." ) should not be "no"	//should be empty substitution; pg. 181
		query( p, "var(X), X = f(Y)." ) should not be "no"
		query( p, "X = f(Y), var(X)." ) shouldBe "no"
		query( p, "var(a)." ) shouldBe "no"
	}
	
	"term comparison" in
	{
	val p = program( """
		""" )
		
		// == 169
		query( p, "f(X,X) == f(X,X)." ) should not be "no"
		query( p, "X = Y, X == Y." ) should not be "no"
		query( p, "1.0e+1 == 10.0" ) shouldBe "yes"
		query( p, "X == Y." ) shouldBe "no"
		query( p, "f(X,X) == f(X,Y)." ) shouldBe "no"
		query( p, "1 == 1.0" ) shouldBe "no"
	}
}