package ca.hyperreal.sprolog

import org.scalatest._
import prop.PropertyChecks

import Prolog.{program, query, queryOnce, emptyProgram}


class Builtins extends FreeSpec with PropertyChecks with Matchers
{
	"logic and control" in
	{
		// call 60
		query( emptyProgram, "X = write( hello ), call( X )." ) shouldBe
			"""	|hello
				|X = write(hello)
				""".stripMargin.trim
	}
	
	"type testing" in
	{
		// arithmetic compare 40
		query( emptyProgram, "X = 1+2, X + 6 =:= X * 3." ) shouldBe "X = +(1, 2)"
		query( emptyProgram, "'=:='(1.0, 1)." ) shouldBe "yes"
		query( emptyProgram, "0.3333333333333333 =:= 1/3." ) shouldBe "no"

		// atom 50
		query( emptyProgram, "atom('Yety')." ) shouldBe "yes"
		query( emptyProgram, "atom([])." ) shouldBe "yes"
		query( emptyProgram, "atom(f(X))." ) shouldBe "no"
		query( emptyProgram, "atom(10.01)." ) shouldBe "no"

		// 90
		query( emptyProgram, "float(10.01)." ) shouldBe "yes"
		query( emptyProgram, "float(-10.01)." ) shouldBe "yes"
//		query( emptyProgram, "float(- -10.01)." ) shouldBe "no"
		query( emptyProgram, "float(10)." ) shouldBe "no"
		query( emptyProgram, "float(X)." ) shouldBe "no"
		query( emptyProgram, "float(a)." ) shouldBe "no"
		
		// atomic 57
		query( emptyProgram, "atomic(10.01)." ) shouldBe "yes"
		query( emptyProgram, "atomic('Yeti')." ) shouldBe "yes"
//		query( emptyProgram, "atomic((;))." ) shouldBe "yes"
		query( emptyProgram, "atomic(X)." ) shouldBe "no"
		query( emptyProgram, "atomic(f(X,Y))." ) shouldBe "no"
		
		// comemptyProgramound 72
		query( emptyProgram, "compound(f(X,Y))." ) should not be "no"
		query( emptyProgram, "compound([a])." ) shouldBe "yes"
		query( emptyProgram, "compound(-a)." ) shouldBe "yes"
		query( emptyProgram, "compound(-1)." ) shouldBe "no"
		query( emptyProgram, "compound(10.01)." ) shouldBe "no"
		query( emptyProgram, "compound('ok')." ) shouldBe "no"
		query( emptyProgram, "compound([])." ) shouldBe "no"
		query( emptyProgram, "compound(A)." ) shouldBe "no"

		// 110
		query( emptyProgram, "integer(10)." ) shouldBe "yes"
		query( emptyProgram, "integer(-10)." ) shouldBe "yes"
//		query( emptyProgram, "integer(- -10)." ) shouldBe "no"
		query( emptyProgram, "integer(10.01)." ) shouldBe "no"
		query( emptyProgram, "integer(X)." ) shouldBe "no"
		query( emptyProgram, "integer('o_k')." ) shouldBe "no"

		// is 111
		query( emptyProgram, "X = 1+2, Y is X*3." ) shouldBe "X = +(1, 2), Y = 9"
		query( emptyProgram, "Result is 3+11.0." ) shouldBe "Result = 14.0"
		query( emptyProgram, "1 is 1.0." ) shouldBe "no"
		
		// number 117
		query( emptyProgram, "number(10.01)." ) shouldBe "yes"
		query( emptyProgram, "number(-10)." ) shouldBe "yes"
		query( emptyProgram, "number('ok')." ) shouldBe "no"
		query( emptyProgram, "number(X)." ) shouldBe "no"
		query( emptyProgram, "number(f(X, Y))." ) shouldBe "no"
		
		// var 181
		query( emptyProgram, "var(X)." ) should not be "no"	//should be empty substitution; pg. 181
		query( emptyProgram, "var(X), X = f(Y)." ) should not be "no"
		query( emptyProgram, "X = f(Y), var(X)." ) shouldBe "no"
		query( emptyProgram, "var(a)." ) shouldBe "no"
	}
	
	"term comparison" in
	{
		// == 169
		query( emptyProgram, "f(X,X) == f(X,X)." ) should not be "no"
		query( emptyProgram, "X = Y, X == Y." ) should not be "no"
		query( emptyProgram, "1.0e+1 == 10.0" ) shouldBe "yes"
		query( emptyProgram, "X == Y." ) shouldBe "no"
		query( emptyProgram, "f(X,X) == f(X,Y)." ) shouldBe "no"
		query( emptyProgram, "1 == 1.0" ) shouldBe "no"
	}
}