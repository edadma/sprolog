package xyz.hyperreal.sprolog

import org.scalatest._
import prop.PropertyChecks

import Prolog.{program, query, queryOnce}


class Misc extends FreeSpec with PropertyChecks with Matchers
{
	"queue" in
	{
	val p = program( """
		createq( q(X, X) ).
		
		createq( Initial, q([Initial|X], X) ).
		
		enqueue( E, q(X, [E|Y]), q(X, Y) ).
		
		dequeue( E, q([E|NewX], Last), q(NewX, Last) ).
		
		emptyq( q(X, Y) ) :- X == Y.
		
		peek( q([X|_], _), X ).
		
		a( E ) :-
			createq( a, Q ),
			enqueue( b, Q, Q1 ),
			enqueue( c, Q1, Q2 ),
			peek( Q2, E ).
		""" )
	
		query( p, "a( E )." ) shouldBe "E = a"
	}
	
	"maplist" in
	{
	val p = program( """
		maplist( _, [], [] ).
		maplist( P, [X|L], [Y|M] ) :-
			Q =.. [P, X, Y], Q, maplist( P, L, M ).
			
		double( A, B ) :- B is 2*A.
		""" )
	
		query( p, "maplist( double, [1, 2, 3], L )." ) shouldBe "L = [2, 4, 6]"
	}
}