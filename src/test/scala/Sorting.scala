package ca.hyperreal.sprolog

import org.scalatest._
import prop.PropertyChecks

import Prolog.{program, query, queryOnce}


class Sorting extends FreeSpec with PropertyChecks with Matchers
{
	"naive_sort" in
	{
	val p = program( """
		naive_sort( List, Sorted ):- permutation( List, Sorted ), is_sorted( Sorted ).
		
		delete( X, [X|R], R ).
		delete( X, [F|R], [F|S] ) :- delete( X, R, S ).
		
		permutation( [], [] ).
		permutation( [X|Y], Z ) :- permutation( Y, W ), delete( X, Z, W ).   

		is_sorted( [] ).
		is_sorted( [_] ).
		is_sorted( [X, Y|T] ) :- X =< Y, is_sorted( [Y|T] ).
		""" )
	
		query( p, "naive_sort( [7, 4, 6, 5, 2, 9], L )." ) shouldBe "L = [2, 4, 5, 6, 7, 9]"
	}
	
	"quick_sort" in
	{
	val p = program( """
		pivoting( H, [], [], [] ).
		pivoting( H, [X|T], [X|L], G ) :- X >= H, pivoting( H, T, L, G ).
		pivoting( H, [X|T], L, [X|G] ) :- X < H, pivoting( H, T, L, G ).
		
		quick_sort( List, Sorted ) :- q_sort( List, [], Sorted ).
		
		q_sort( [], Acc, Acc ).
		q_sort( [H|T], Acc, Sorted ):-
			pivoting( H, T, L1, L2 ),
			q_sort( L1, Acc, Sorted1 ), q_sort( L2, [H|Sorted1], Sorted ).
		""" )
	
		query( p, "quick_sort( [7, 4, 6, 5, 2, 9], L )." ) shouldBe "L = [2, 4, 5, 6, 7, 9]"
	}
	
	"merge_sort" in
	{
	val p = program( """
		halve( L, A, B ) :- hv( L, L, A, B ).
		
		hv( [], R, [], R ).   % for lists of even length
		hv( [_], R, [], R ).  % for lists of odd length
		hv( [_, _|T], [X|L], [X|L1], R ) :- hv( T, L, L1, R ).
		
		merge_sort( [], [] ).     % empty list is already sorted
		merge_sort( [X], [X] ).   % single element list is already sorted
		merge_sort( List, Sorted ):-
			List = [_,_|_], halve( List, L1, L2 ),				% list with at least two elements is divided into two parts
			merge_sort( L1, Sorted1 ), merge_sort( L2, Sorted2 ),	% then each part is sorted
			merge( Sorted1, Sorted2, Sorted ).					% and sorted parts are merged
		merge( [], L, L ).
		merge( L, [], L ) :- L \= [].
		merge( [X|T1], [Y|T2], [X|T] ) :- X =< Y, merge( T1, [Y|T2], T ).
		merge( [X|T1], [Y|T2], [Y|T] ) :- X > Y, merge( [X|T1], T2, T ).
		""" )
	
		query( p, "merge_sort( [7, 4, 6, 5, 2, 9], L )." ) shouldBe "L = [2, 4, 5, 6, 7, 9]"
	}
}