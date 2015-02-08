package ca.hyperreal.sprolog

import org.scalatest._
import prop.PropertyChecks

import Prolog.{program, query, queryOnce}


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
		query( Database.empty, "X = write( hello ), call( X )." ) shouldBe "helloX = write(hello)"
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
		query( Database.empty, "functor( asdf, A, B )." ) shouldBe "A = asdf, B = 0"
		query( Database.empty, "functor( 1.5, A, B )." ) shouldBe "A = 1.5, B = 0"
		query( Database.empty, "functor( A, asdf, 0 )." ) shouldBe "A = asdf"
		query( Database.empty, "functor( A, 1.5, 0 )." ) shouldBe "A = 1.5"
		query( Database.empty, "functor( foo(aa, X), Y, Z )." ) should fullyMatch regex """X = H\d+, Y = foo, Z = 2"""
		query( Database.empty, "functor( X, foo, 3 )." ) should fullyMatch regex """X = foo\(H\d+, H\d+, H\d+\)"""
		query( Database.empty, "functor( F, 1.5, 1 )." ) shouldBe "no"
	}
	
	"term comparison" in
	{
		// == 169
		query( Database.empty, "f(X,X) == f(X,X)." ) should not be "no"
		query( Database.empty, "X = Y, X == Y." ) should not be "no"
		query( Database.empty, "1.0e+1 == 10.0" ) shouldBe "yes"
		query( Database.empty, "X == Y." ) shouldBe "no"
		query( Database.empty, "f(X,X) == f(X,Y)." ) shouldBe "no"
		query( Database.empty, "1 == 1.0" ) shouldBe "no"
	}
	
	"type testing" in
	{
		// arithmetic compare 40
		query( Database.empty, "X = 1+2, X + 6 =:= X * 3." ) shouldBe "X = +(1, 2)"
		query( Database.empty, "'=:='(1.0, 1)." ) shouldBe "yes"
		query( Database.empty, "0.3333333333333333 =:= 1/3." ) shouldBe "no"

		// atom 50
		query( Database.empty, "atom('Yety')." ) shouldBe "yes"
		query( Database.empty, "atom([])." ) shouldBe "yes"
		query( Database.empty, "atom(f(X))." ) shouldBe "no"
		query( Database.empty, "atom(10.01)." ) shouldBe "no"
		
		// atomic 57
		query( Database.empty, "atomic(10.01)." ) shouldBe "yes"
		query( Database.empty, "atomic('Yeti')." ) shouldBe "yes"
//		query( Database.empty, "atomic((;))." ) shouldBe "yes"
		query( Database.empty, "atomic(X)." ) shouldBe "no"
		query( Database.empty, "atomic(f(X,Y))." ) shouldBe "no"
		
		// compound 72
		query( Database.empty, "compound(f(X,Y))." ) should not be "no"
		query( Database.empty, "compound([a])." ) shouldBe "yes"
		query( Database.empty, "compound(-a)." ) shouldBe "yes"
		query( Database.empty, "compound(-1)." ) shouldBe "no"
		query( Database.empty, "compound(10.01)." ) shouldBe "no"
		query( Database.empty, "compound('ok')." ) shouldBe "no"
		query( Database.empty, "compound([])." ) shouldBe "no"
		query( Database.empty, "compound(A)." ) shouldBe "no"

		// fail 87
		val p = program( """
			F ; _ :- F.
			_ ; A :- A.
			""" )
		
		query( p, "(X = 1 ; X = 2), write(X), fail." ) shouldBe "12no"
		
		// float 90
		query( Database.empty, "float(10.01)." ) shouldBe "yes"
		query( Database.empty, "float(-10.01)." ) shouldBe "yes"
//		query( Database.empty, "float(- -10.01)." ) shouldBe "no"
		query( Database.empty, "float(10)." ) shouldBe "no"
		query( Database.empty, "float(X)." ) shouldBe "no"
		query( Database.empty, "float(a)." ) shouldBe "no"

		// integer 110
		query( Database.empty, "integer(10)." ) shouldBe "yes"
		query( Database.empty, "integer(-10)." ) shouldBe "yes"
//		query( Database.empty, "integer(- -10)." ) shouldBe "no"
		query( Database.empty, "integer(10.01)." ) shouldBe "no"
		query( Database.empty, "integer(X)." ) shouldBe "no"
		query( Database.empty, "integer('o_k')." ) shouldBe "no"

		// is 111
		query( Database.empty, "X = 1+2, Y is X*3." ) shouldBe "X = +(1, 2), Y = 9"
		query( Database.empty, "Result is 3+11.0." ) shouldBe "Result = 14.0"
		query( Database.empty, "1 is 1.0." ) shouldBe "no"
		
		// number 117
		query( Database.empty, "number(10.01)." ) shouldBe "yes"
		query( Database.empty, "number(-10)." ) shouldBe "yes"
		query( Database.empty, "number('ok')." ) shouldBe "no"
		query( Database.empty, "number(X)." ) shouldBe "no"
		query( Database.empty, "number(f(X, Y))." ) shouldBe "no"
		
		// var 181
		query( Database.empty, "var(X)." ) should not be "no"	//should be empty substitution; pg. 181
		query( Database.empty, "var(X), X = f(Y)." ) should not be "no"
		query( Database.empty, "X = f(Y), var(X)." ) shouldBe "no"
		query( Database.empty, "var(a)." ) shouldBe "no"
	}
}