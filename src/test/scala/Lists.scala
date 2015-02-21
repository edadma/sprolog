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
		query( db, "subset( [b, c], [d, a, c, b] )." ) shouldBe "yes"
		query( db, "subset( [e, b, c], [d, a, c, b] )." ) shouldBe "no"
	}
	
	"sum_list" in
	{
		query( db, "sum_list( [4, 5, 6], S )." ) shouldBe "S = 15"
		query( db, "sum_list( [], S )." ) shouldBe "S = 0"
	}
	
	"is_list" in
	{
		query( db, "is_list( [4, 5, 6] )." ) shouldBe "yes"
		query( db, "is_list( [] )." ) shouldBe "yes"
		query( db, "is_list( 4 )." ) shouldBe "no"
	}
	
	"length" in
	{
		query( db, "length( [4, 5, 6], L )." ) shouldBe "L = 3"
		query( db, "length( [], L )." ) shouldBe "L = 0"
	}
	
	"intersection" in
	{
		queryOnce( db, "intersection( [a, b, c, d], [b, c, d, e], L )." ) shouldBe "L = [b, c, d]"
	}
	
	"powerset" in
	{
		queryOnce( db, "powerset([1,2,3], P)." ) shouldBe "P = [[], [3], [2], [3, 2], [1], [3, 1], [2, 1], [3, 2, 1]]"
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

	"sublist" in
	{
		query( db, "sublist( [b, c], [a, b, c, d] )." ) shouldBe "yes"
		query( db, "sublist( [c, b], [a, b, c, d] )." ) shouldBe "no"
		query( db, "sublist( [b, A], [a, b, c, b, d] )." ) shouldBe
			"""	|A = c
				|A = d
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
