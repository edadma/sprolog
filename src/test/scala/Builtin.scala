package ca.hyperreal.sprolog

import org.scalatest._
import prop.PropertyChecks

import Prolog.{program, query, queryOnce, db}


class Builtin extends FreeSpec with PropertyChecks with Matchers
{
	"logic and control" in
	{
	val p = program( """
		insect( bee ).
		insect( ant ) :- !.
		insect( beetle ).
		animal( horse ).
		animal( cat ).
		animal( dog ).
		""" )
			
		// call 60
 		query( p, "X = write( hello ), call( X )." ) shouldBe "helloX = write(hello)"
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
		// arg 39
 		query( db, "arg(2, foo(a, b, c), X)." ) shouldBe "X = b"
		query( db, "arg(2, foo(a, f(X, b), c), f(a, Y))." ) shouldBe "X = a, Y = b"
		query( db, "arg(1, foo(a, b, c), b)." ) shouldBe "no"
		
		// functor 93
 		query( db, "functor( asdf, A, B )." ) shouldBe "A = asdf, B = 0"
 		query( db, "functor( 1.5, A, B )." ) shouldBe "A = 1.5, B = 0"
 		query( db, "functor( A, asdf, 0 )." ) shouldBe "A = asdf"
 		query( db, "functor( A, 1.5, 0 )." ) shouldBe "A = 1.5"
 		query( db, "functor( foo(aa, X), Y, Z )." ) should fullyMatch regex """X = H\d+, Y = foo, Z = 2"""
		query( db, "functor( X, foo, 3 )." ) should fullyMatch regex """X = foo\(H\d+, H\d+, H\d+\)"""
		query( db, "functor( F, 1.5, 1 )." ) shouldBe "no"
		
		// univ 179
 		query( db, "A =.. [f, a]." ) shouldBe "A = f(a)"
		query( db, "A =.. [f]." ) shouldBe "A = f"
		query( db, "A =.. [f(a)]." ) shouldBe "no"
		query( db, "A =.. [1.5]." ) shouldBe "A = 1.5"
		query( db, "A =.. [f, g(b), 1]." ) shouldBe "A = f(g(b), 1)"
 		query( db, "a =.. [a]." ) shouldBe "yes"
		query( db, "1.5 =.. [1.5]." ) shouldBe "yes"
		query( db, "a =.. A." ) shouldBe "A = [a]"
		query( db, "foo(a, b) =.. [foo, a, b]." ) shouldBe "yes"
		query( db, "foo(a, b) =.. [foo, b, a]." ) shouldBe "no"
		query( db, "foo(a, b) =.. [A|B]." ) shouldBe "A = foo, B = [a, b]"
	}
	
	"term comparison" in
	{
		// == 169
		query( db, "f(X,X) == f(X,X)." ) should not be "no"
		query( db, "X = Y, X == Y." ) should not be "no"
		query( db, "1.0e+1 == 10.0" ) shouldBe "yes"
		query( db, "X == Y." ) shouldBe "no"
		query( db, "f(X,X) == f(X,Y)." ) shouldBe "no"
		query( db, "1 == 1.0" ) shouldBe "no"
	}
	
	"type testing" in
	{
		// arithmetic compare 40
		query( db, "X = 1+2, X + 6 =:= X * 3." ) shouldBe "X = 1 + 2"
		query( db, "'=:='(1.0, 1)." ) shouldBe "yes"
		query( db, "0.3333333333333333 =:= 1/3." ) shouldBe "no"

		// atom 50
		query( db, "atom('Yety')." ) shouldBe "yes"
		query( db, "atom([])." ) shouldBe "yes"
		query( db, "atom(f(X))." ) shouldBe "no"
		query( db, "atom(10.01)." ) shouldBe "no"
		
		// atomic 57
		query( db, "atomic(10.01)." ) shouldBe "yes"
		query( db, "atomic('Yeti')." ) shouldBe "yes"
//		query( db, "atomic((;))." ) shouldBe "yes"
		query( db, "atomic(X)." ) shouldBe "no"
		query( db, "atomic(f(X,Y))." ) shouldBe "no"
		
		// compound 72
		query( db, "compound(f(X,Y))." ) should not be "no"
		query( db, "compound([a])." ) shouldBe "yes"
		query( db, "compound(-a)." ) shouldBe "yes"
		query( db, "compound(-1)." ) shouldBe "no"
		query( db, "compound(10.01)." ) shouldBe "no"
		query( db, "compound('ok')." ) shouldBe "no"
		query( db, "compound([])." ) shouldBe "no"
		query( db, "compound(A)." ) shouldBe "no"

		// fail 87
		query( db, "(X = 1 ; X = 2), write(X), fail." ) shouldBe "12no"
		
		// float 90
		query( db, "float(10.01)." ) shouldBe "yes"
		query( db, "float(-10.01)." ) shouldBe "yes"
//		query( db, "float(- -10.01)." ) shouldBe "no"
		query( db, "float(10)." ) shouldBe "no"
		query( db, "float(X)." ) shouldBe "no"
		query( db, "float(a)." ) shouldBe "no"

		// integer 110
		query( db, "integer(10)." ) shouldBe "yes"
		query( db, "integer(-10)." ) shouldBe "yes"
//		query( db, "integer(- -10)." ) shouldBe "no"
		query( db, "integer(10.01)." ) shouldBe "no"
		query( db, "integer(X)." ) shouldBe "no"
		query( db, "integer('o_k')." ) shouldBe "no"

		// is 111
		query( db, "X = 1+2, Y is X*3." ) shouldBe "X = 1 + 2, Y = 9"
		query( db, "Result is 3+11.0." ) shouldBe "Result = 14.0"
		query( db, "1 is 3 - 2." ) shouldBe "yes"
		query( db, "A = 1, A is 3 - 2." ) shouldBe "A = 1"
		query( db, "1 is 1.0." ) shouldBe "no"
		
		// number 117
		query( db, "number(10.01)." ) shouldBe "yes"
		query( db, "number(-10)." ) shouldBe "yes"
		query( db, "number('ok')." ) shouldBe "no"
		query( db, "number(X)." ) shouldBe "no"
		query( db, "number(f(X, Y))." ) shouldBe "no"
		
		// var 181
		query( db, "var(X)." ) should not be "no"	//should be empty substitution; pg. 181
		query( db, "var(X), X = f(Y)." ) should not be "no"
		query( db, "X = f(Y), var(X)." ) shouldBe "no"
		query( db, "var(a)." ) shouldBe "no"
	}
}