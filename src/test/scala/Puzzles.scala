package ca.hyperreal.sprolog

import org.scalatest._
import prop.PropertyChecks

import Prolog.{program, query, queryOnce}


class Puzzles extends FreeSpec with PropertyChecks with Matchers
{
	"zebra" in
	{
	val p = program( """		
		sublist([], L).
		
		sublist([H|T], [H|U] ) :- initialsublist(T,U). % 2
		initialsublist([], L). % 3
		initialsublist([H|T],[H|U] ) :- initialsublist(T,U). 

		sublist(S, [H|U] ) :- sublist(S,U).

		next(A,B,L):- sublist([A,B],L).
		next(A,B,L):- sublist([B,A],L).

		zebra( Zebra, Water ) :-
			L = [h(H1,N1,P1,D1,C1),
			h(H2,N2,P2,D2,C2),
			h(H3,N3,P3,D3,C3),
			h(H4,N4,P4,D4,C4),
			h(H5,N5,P5,D5,C5)],
			member(h(red,english,_,_,_),L), % 2
			member(h(_,spanish,dog,_,_),L), % 3
			member(h(green,_,_,coffee,_),L), % 4
			member(h(_,ukrainian,_,tea,_),L), % 5
			sublist([h(ivory,_,_,_,_),h(green,_,_,_,_)],L), % 6
			member(h(_,_,snails,_,old_gold),L), % 7
			member(h(yellow,_,_,_,kool),L), % 8
			L = [_,_,h(_,_,_,milk,_),_,_], % 9
			L = [h(_,norwegian,_,_,_)|_], % 10
			next(h(_,_,_,_,chesterfields),h(_,_,fox,_,_),L), % 11
			next(h(_,_,_,_,kool),h(_,_,horse,_,_),L), % 12
			member(h(_,_,_,orange,lucky_strike),L), % 13
			member(h(_,japanese,_,_,parliaments),L), % 14
			next(h(_,norwegian,_,_,_),h(blue,_,_,_,_),L), % 15
			member(h(_,Zebra,zebra,_,_),L), % Q1
			member(h(_,Water,_,water,_),L). % Q2
		""" )
	
		query( p, "zebra( Zebra, Water )." ) shouldBe "Water = norwegian, Zebra = japanese"
	}
}