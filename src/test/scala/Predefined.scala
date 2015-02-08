package ca.hyperreal.sprolog

import org.scalatest._
import prop.PropertyChecks

import Prolog.{program, query, queryOnce, emptyProgram}


class Predefined extends FreeSpec with PropertyChecks with Matchers
{
	"logic and control" in
	{
	val p = program( """
		C -> T :- C, !, T.
		
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
	}
}