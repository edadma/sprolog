package ca.hyperreal.sprolog

import org.scalatest._
import prop.PropertyChecks

import Prolog.{program, query, queryOnce, emptyProgram}


class Predefined extends FreeSpec with PropertyChecks with Matchers
{
	"logic and control" in
	{
	val p = program( """
		If -> Then        :- If, !, Then.
		If -> Then ; _    :- If, !, Then.
		_  -> _    ; Else :- Else, !.
		
		F ; _ :- F.
		_ ; A :- A.
		
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
	}
}