package ca.hyperreal.sprolog

import org.scalatest._
import prop.PropertyChecks

import Prolog.{program, query, queryOnce}


class Puzzles extends FreeSpec with PropertyChecks with Matchers
{
	"zebra" in
	{
	val p = program( """		
		next( A, B, L ) :- sublist( [A, B], L ).
		next( A, B, L ) :- sublist( [B, A], L ).
%		next( A, B, L ) :- sublist( [A, B], L ) ; sublist( [B, A], L ).

		zebra( Zebra, Water ) :-
			length( L, 5 ), !, 												%  1
			member( [red, english, _, _, _], L ),							%  2
			member( [_, spanish, dog, _, _], L ),							%  3
			member( [green, _, _, coffee, _] , L ),							%  4
			member( [_, ukrainian, _, tea, _] , L ),						%  5
			sublist( [[ivory, _, _, _, _], [green, _, _, _, _]], L ),		%  6
			member( [_, _, snails, _, old_gold] , L ),						%  7
			member( [yellow, _, _, _, kool] , L ),							%  8
			L = [_, _, [_, _, _, milk, _] , _, _],							%  9
			L = [[_, norwegian, _, _, _]|_],								% 10
			next( [_, _, _, _, chesterfields], [_, _, fox, _, _], L ),		% 11
			next( [_, _, _, _, kool], [_, _, horse, _, _], L ),				% 12
			member( [_, _, _, orange, lucky_strike], L ),					% 13
			member( [_, japanese, _, _, parliaments], L ),					% 14
			next( [_, norwegian, _, _, _], [blue, _, _, _, _], L ),			% 15
			member( [_, Zebra, zebra, _, _], L ),							% Q1
			member( [_, Water, _, water, _], L ).							% Q2
		""" )
	
		query( p, "zebra( Zebra, Water )." ) shouldBe "Water = norwegian, Zebra = japanese"
	}
}
// 			member(h(red, english, _, _, _), L ), % 2
// 			member(h(_, spanish, dog, _, _), L ), % 3
// 			member(h(green, _, _, coffee, _), L ), % 4
// 			member(h(_, ukrainian, _, tea, _), L ), % 5
// 			sublist([h(ivory, _, _, _, _), h(green, _, _, _, _)], L ), % 6
// 			member(h(_, _, snails, _, old_gold), L ), % 7
// 			member(h(yellow, _, _, _, kool), L ), % 8
// 			L = [_, _, h(_, _, _, milk, _), _, _], % 9
// 			L = [h(_, norwegian, _, _, _)|_], % 10
// 			next(h(_, _, _, _, chesterfields), h(_, _, fox, _, _), L ), % 11
// 			next(h(_, _, _, _, kool), h(_, _, horse, _, _), L ), % 12
// 			member(h(_, _, _, orange, lucky_strike), L ), % 13
// 			member(h(_, japanese, _, _, parliaments), L ), % 14
// 			next(h(_, norwegian, _, _, _), h(blue, _, _, _, _), L ), % 15
// 			member(h(_, Zebra, zebra, _, _), L ), % Q1
// 			member(h(_, Water, _, water, _), L ). % Q2
/*
			member([red, english, _, _, _], L ), % 2
			member([_, spanish, dog, _, _], L ), % 3
			member([green, _, _, coffee, _], L ), % 4
			member([_, ukrainian, _, tea, _], L ), % 5
			sublist([[ivory, _, _, _, _], [green, _, _, _, _], L ), % 6
			member([_, _, snails, _, old_gold], L ), % 7
			member([yellow, _, _, _, kool], L ), % 8
			L = [_, _, [_, _, _, milk, _], _, _], % 9
			L = [[_, norwegian, _, _, _]|_], % 10
			next([_, _, _, _, chesterfields], [_, _, fox, _, _], L ), % 11
			next([_, _, _, _, kool], [_, _, horse, _, _], L ), % 12
			member([_, _, _, orange, lucky_strike], L ), % 13
			member([_, japanese, _, _, parliaments], L ), % 14
			next([_, norwegian, _, _, _], [blue, _, _, _, _], L ), % 15
			member([_, Zebra, zebra, _, _], L ), % Q1
			member([_, Water, _, water, _], L ). % Q2
*/