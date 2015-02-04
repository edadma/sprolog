package ca.hyperreal.sprolog

import org.scalatest._
import prop.PropertyChecks

import Prolog.{program, query, queryFirst}


class Lists extends FreeSpec with PropertyChecks with Matchers
{
	"member" in
	{
	val p = program( """
		X = X.
		
		member( X, [X|_] ).
		member( X, [_|T] ) :- member( X, T ).
		""" )
	
		query( p, "member( d, [a, b, c] )." ) shouldBe "no"
		query( p, "member( b, [a, b, c] )." ) shouldBe "yes"
 		query( p, "member( E, [a, b, c] )." ) shouldBe
 			"""	|E = a
				|E = b
				|E = c
				""".stripMargin.trim
 		query( p, "L1 = [a, b, c, e], L2 = [a, c, d, e], member( M, L1 ), member( M, L2 )." ) shouldBe
 			"""	|L1 = [a, b, c, e], L2 = [a, c, d, e], M = a
				|L1 = [a, b, c, e], L2 = [a, c, d, e], M = c
				|L1 = [a, b, c, e], L2 = [a, c, d, e], M = e
				""".stripMargin.trim
	}
	
	"subset" in
	{
	val p = program( """
		member( X, [X|_] ).
		member( X, [_|T] ) :- member( X, T ).
		
		subset( [], _ ).
		subset( [H|T], L ) :-
			member( H, L ),
			subset( T, L ).
		""" )
	
		query( p, "subset( [b, c], [d, a, c, b] )." ) shouldBe "yes"
		query( p, "subset( [e, b, c], [d, a, c, b] )." ) shouldBe "no"
	}
	
	"intersection" in
	{
	val p = program( """
		member( X, [X|_] ).
		member( X, [_|T] ) :- member( X, T ).
		
		intersection( [], _, [] ).
		intersection( [H|T1], L, [H|T2] ) :- member( H, L ), intersection( T1, L, T2 ).
		intersection( [_|T1], L, T2 ) :- intersection( T1, L, T2 ).
		""" )
	
		queryFirst( p, "intersection( [a, b, c, d], [b, c, d, e], L )." ) shouldBe "L = [b, c, d]"
	}
	
	"permutation" in
	{
	val p = program( """
		delete( X, [X|R], R ).
		delete( X, [F|R], [F|S] ) :- delete( X, R, S ).
		
		permutation( [], [] ).
		permutation( [X|Y], Z ) :- permutation( Y, W ), delete( X, Z, W ).   
		""" )
	
		query( p, "permutation( [a, b, c], [b, c, a] )." ) shouldBe "yes"
		query( p, "permutation( [a, b, c], [c, a] )." ) shouldBe "no"
		query( p, "permutation( [a, b, c], P )." ) shouldBe
			"""
				|P = [a, b, c]
				|P = [b, a, c]
				|P = [b, c, a]
				|P = [a, c, b]
				|P = [c, a, b]
				|P = [c, b, a]
				""".stripMargin.trim
	}
	
	"reverse" in
	{
	val p = program( """
		reverse( List, Reversed ) :- reverse( List, [], Reversed ).

		reverse( [], Reversed, Reversed ).
		reverse( [Head|Tail], SoFar, Reversed ) :- reverse( Tail, [Head|SoFar], Reversed ).
		""" )
	
		query( p, "reverse( [a, b, c], L )." ) shouldBe "L = [c, b, a]"
	}
	
	"append" in
	{
	val p = program( """
		append( [], L, L ).
		append( [H|T], L, [H|LT] ):-append( T, L, LT ).
		""" )
	
		query( p, "append( [a, b, c], [d, e, f], [a, b, c, d, e, f] )." ) shouldBe "yes"
		query( p, "append( [], [d, e, f], L )." ) shouldBe "L = [d, e, f]"
		query( p, "append( [a, b, c], [], L )." ) shouldBe "L = [a, b, c]"
		query( p, "append( [a, b], [d, e, f], L )." ) shouldBe "L = [a, b, d, e, f]"
		query( p, "append( [a, b, c], [d, e], L )." ) shouldBe "L = [a, b, c, d, e]"
		query( p, "append( [a, b, c], L, [a, b, c, d, e, f] )." ) shouldBe "L = [d, e, f]"
	}
}
