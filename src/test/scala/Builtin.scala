package ca.hyperreal.sprolog

import org.scalatest._
import prop.PropertyChecks

import Prolog.{program, query, queryOnce, emptyProgram}


class Builtin extends FreeSpec with PropertyChecks with Matchers
{
	"logic and control" in
	{
	val p = program( """
		F ; _ :- F.
		_ ; A :- A.
		
		insect( bee ).
		insect( ant ) :- !.
		insect( beetle ).
		animal( horse ).
		animal( cat ).
		animal( dog ).
		""" )
			
		// call 60
		query( emptyProgram, "X = write( hello ), call( X )." ) shouldBe "helloX = write(hello)"
		query( p, "call(!) ; true." ) shouldBe "yes"
		query( p, "(X = 1; X = 2), call(!)." ) shouldBe
			"""	|X = 1
				|X = 2
				""".stripMargin.trim			
		
		// ! 85
		query( p, "insect(X)." ) shouldBe
			"""	|X = bee
				|X = ant
				""".stripMargin.trim			
		query( p, "insect(X), !." ) shouldBe "X = bee"
		query( p, "(insect(X); animal(Y)), !." ) should fullyMatch regex """X = bee, Y = H\d+"""
		query( p, "insect(X), !, animal(Y)." ) shouldBe
			"""	|X = bee, Y = horse
				|X = bee, Y = cat
				|X = bee, Y = dog
				""".stripMargin.trim			
	}
	
	"term creation and decomposition" in
	{
		query( emptyProgram, "functor( asdf, A, B )." ) shouldBe "A = asdf, B = 0"
		query( emptyProgram, "functor( 1.5, A, B )." ) shouldBe "A = 1.5, B = 0"
		query( emptyProgram, "functor( A, asdf, 0 )." ) shouldBe "A = asdf"
		query( emptyProgram, "functor( A, 1.5, 0 )." ) shouldBe "A = 1.5"
		query( emptyProgram, "functor( foo(aa, X), Y, Z )." ) should fullyMatch regex """X = H\d+, Y = foo, Z = 2"""
		query( emptyProgram, "functor( X, foo, 3 )." ) should fullyMatch regex """X = foo\(H\d+, H\d+, H\d+\)"""
		query( emptyProgram, "functor( F, 1.5, 1 )." ) shouldBe "no"
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
		
		// atomic 57
		query( emptyProgram, "atomic(10.01)." ) shouldBe "yes"
		query( emptyProgram, "atomic('Yeti')." ) shouldBe "yes"
//		query( emptyProgram, "atomic((;))." ) shouldBe "yes"
		query( emptyProgram, "atomic(X)." ) shouldBe "no"
		query( emptyProgram, "atomic(f(X,Y))." ) shouldBe "no"
		
		// compound 72
		query( emptyProgram, "compound(f(X,Y))." ) should not be "no"
		query( emptyProgram, "compound([a])." ) shouldBe "yes"
		query( emptyProgram, "compound(-a)." ) shouldBe "yes"
		query( emptyProgram, "compound(-1)." ) shouldBe "no"
		query( emptyProgram, "compound(10.01)." ) shouldBe "no"
		query( emptyProgram, "compound('ok')." ) shouldBe "no"
		query( emptyProgram, "compound([])." ) shouldBe "no"
		query( emptyProgram, "compound(A)." ) shouldBe "no"

		// fail 87
		val p = program( """
			F ; _ :- F.
			_ ; A :- A.
			""" )
		
		query( p, "(X = 1 ; X = 2), write(X), fail." ) shouldBe "12no"
		
		// float 90
		query( emptyProgram, "float(10.01)." ) shouldBe "yes"
		query( emptyProgram, "float(-10.01)." ) shouldBe "yes"
//		query( emptyProgram, "float(- -10.01)." ) shouldBe "no"
		query( emptyProgram, "float(10)." ) shouldBe "no"
		query( emptyProgram, "float(X)." ) shouldBe "no"
		query( emptyProgram, "float(a)." ) shouldBe "no"

		// integer 110
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
}