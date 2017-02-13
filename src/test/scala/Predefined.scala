package xyz.hyperreal.sprolog

import org.scalatest._
import prop.PropertyChecks

import Prolog.{program, query, queryOnce}


class Predefined extends FreeSpec with PropertyChecks with Matchers
{
	"logic and control" in
	{
	val p = program( """		
		legs( A, 6 ) :- insect( A ).
		legs( horse, 4 ).
		insect( bee ).
		insect( ant ).
		fly( bee ).
		""" )

		// ; 86
		query( p, "(insect(X), fly(X)) ; (legs(X, 6), fly(X))." ) shouldBe
			"""	|X = bee
				|X = bee
				""".stripMargin.trim
		query( p, "X = 123, (A = 1 ; A = 2)." ) shouldBe
			"""	|A = 1, X = 123
				|A = 2, X = 123
				""".stripMargin.trim
				
		// -> 106
		query( p, "X = 0 -> write('null')." ) shouldBe "nullX = 0"
		query( p, "legs(A, 6) -> write(insect(A))." ) shouldBe "insect(bee)A = bee"
		query( p, """X \= 0 -> write('positive').""" ) shouldBe "no"
		query( p, "fail -> (true ; true)." ) shouldBe "no"
		
		// -> ; 108
		query( p, "X = 0 -> write('null'); write('positive')." ) shouldBe "nullX = 0"
		query( p, "X = 1, (X = 0 -> write('null'); write('positive'))." ) shouldBe "positiveX = 1"
		query( p, "((!, X = 1, fail) -> true; fail); X = 2." ) shouldBe "X = 2"
		query( p, "fail -> true ; true." ) shouldBe "yes"
		query( p, "(!, X = 1, fail) -> true; fail." ) shouldBe "no"
		
		// \+ 115
		query( p, """X = 3, \+ (X = 1 ; X = 2).""" ) shouldBe "X = 3"
		query( p, """\+ fail.""" ) shouldBe "yes"
		query( p, """\+ !; X = 1.""" ) shouldBe "X = 1"
		query( p, """\+ (X = 1 ; X = 2), X = 3.""" ) shouldBe "no"
		query( p, """X = 1, \+ (X = 1 ; X = 2).""" ) shouldBe "no"
		a [RuntimeException] should be thrownBy {query( p, """\+ (fail, 1).""" )}
		
		// once 122
		query( p, "once( X = 1 ; X = 2 )." ) shouldBe "X = 1"
		query( p, "once( repeat )." ) shouldBe "yes"
		query( p, "once( fail )." ) shouldBe "no"
		
		// repeat 153
		query( p, "repeat, !." ) shouldBe "yes"
		query( p, "repeat, !, fail." ) shouldBe "no"
		a [NoSuchElementException] should be thrownBy {query( p, "iterator_( [1, 2, 3], I ), repeat, next_( I, _ ), fail." )}
	}
}