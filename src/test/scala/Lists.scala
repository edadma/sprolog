package ca.hyperreal.sprolog

import org.scalatest._
import prop.PropertyChecks

import Prolog.{program, query, queryOnce, db}


class Lists extends FreeSpec with PropertyChecks with Matchers
{
	"member" in
	{
		query( db, "member( d, [a, b, c] )." ) shouldBe "no"
		query( db, "member( b, [a, b, c] )." ) shouldBe "yes"
 		query( db, "member( E, [a, b, c] )." ) shouldBe
 			"""	|E = a
				|E = b
				|E = c
				""".stripMargin.trim
 		query( db, "L1 = [a, b, c, e], L2 = [a, c, d, e], member( M, L1 ), member( M, L2 )." ) shouldBe
 			"""	|L1 = [a, b, c, e], L2 = [a, c, d, e], M = a
				|L1 = [a, b, c, e], L2 = [a, c, d, e], M = c
				|L1 = [a, b, c, e], L2 = [a, c, d, e], M = e
				""".stripMargin.trim
	}
	
	"subset" in
	{
	val p = program( """
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
		intersection( [], _, [] ).
		intersection( [H|T1], L, [H|T2] ) :- member( H, L ), intersection( T1, L, T2 ).
		intersection( [_|T1], L, T2 ) :- intersection( T1, L, T2 ).
		""" )
	
		queryOnce( p, "intersection( [a, b, c, d], [b, c, d, e], L )." ) shouldBe "L = [b, c, d]"
	}
	
	"permutation" in
	{
		query( db, "permutation( [a, b, c], [b, c, a] )." ) shouldBe "yes"
		query( db, "permutation( [a, b, c], [c, a] )." ) shouldBe "no"
		query( db, "permutation( [a, b, c], P )." ) shouldBe
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
		query( db, "reverse( [a, b, c], L )." ) shouldBe "L = [c, b, a]"
	}
	
	"append" in
	{
		query( db, "append( [a, b, c], [d, e, f], [a, b, c, d, e, f] )." ) shouldBe "yes"
		query( db, "append( [], [d, e, f], L )." ) shouldBe "L = [d, e, f]"
		query( db, "append( [a, b, c], [], L )." ) shouldBe "L = [a, b, c]"
		query( db, "append( [a, b], [d, e, f], L )." ) shouldBe "L = [a, b, d, e, f]"
		query( db, "append( [a, b, c], [d, e], L )." ) shouldBe "L = [a, b, c, d, e]"
		query( db, "append( [a, b, c], L, [a, b, c, d, e, f] )." ) shouldBe "L = [d, e, f]"
	}
}
