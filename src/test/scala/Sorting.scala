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
	
	"heap_sort" in
	{
	val p = program( """
		/* heap_sort(Xs, Ys) is true if Ys is a sorted permutation of the list Xs. */
		heap_sort(List, SortedList):-
		list_heap(List, Heap),
		heap_list(Heap, SortedList).

		/* list_heap(List, Heap) is true if Heap is the result of inserting        */
		/*   successive elements of the List into an empty heap.                   */
		list_heap(List, Heap):-list_heap_1(List, void, Heap).

		list_heap_1([], Heap, Heap).
		list_heap_1([X|Xs], Heap0, Heap):-
		insert_heap(X, Heap0, Heap1),
		list_heap_1(Xs, Heap1, Heap).

		/* heap_list(Heap, List) is true if List is the result of removing         */
		/*   successive elements from the Heap.                                    */
		heap_list(void, []):-!.
		heap_list(Heap, [X|Xs]):-
		remove_heap(X, Heap, Heap1),
		heap_list(Heap1, Xs).

		/* insert_heap(Value, Heap1, Heap2) is true if inserting Value into Heap1  */
		/*   gives Heap2.                                                          */
		insert_heap(Value, heap(Top, Left, Right), NewHeap):-
		Value < Top, !,
		NewHeap = heap(Top, Right, Left1),
		insert_heap(Value, Left, Left1).
		insert_heap(Value, heap(Top, Left, Right), heap(Value, Right, Left1)):-
		!,
		insert_heap(Top, Left, Left1).
		insert_heap(Value, void, heap(Value, void, void)).

		/* remove_heap(Top, Heap1, Heap2) is true if removing Top from Heap1       */
		/*   gives Heap2.                                                          */
		remove_heap(Top, heap(Top, void, Right), Right):-!.
		remove_heap(Top, heap(Top, Left, Right), NewHeap):-
		remove_heap_1(Value1, Right, Right1),
		heap(heap(Value1, Right1, Left), NewHeap).

		remove_heap_1(Top, heap(Top, void, Right), Right):-!.
		remove_heap_1(Value, heap(Top, Left, Right), heap(Top, Left1, Left)):-
		remove_heap_1(Value, Right, Left1).

		/* heap(Heap1, Heap2) is true if Heap2 is the heap derived from the        */
		/*   pseudo-heap Heap1.                                                    */
		heap(heap(Top, heap(LeftValue, LeftLeft, LeftRight),
					heap(RightValue, RightLeft, RightRight)), NewHeap):-
		RightValue < LeftValue, Top < LeftValue, !,
		NewHeap = heap(LeftValue, Left, heap(RightValue, RightLeft, RightRight)),
		heap(heap(Top, LeftLeft, LeftRight), Left).
		heap(heap(Top, Left, heap(RightValue, RightLeft, RightRight)), NewHeap):-
		Top < RightValue, !,
		NewHeap = heap(RightValue, Left, Right),
		heap(heap(Top, RightLeft, RightRight), Right).
		heap(Heap, Heap).
		""" )
	
		query( p, "heap_sort( [7, 4, 6, 5, 2, 9], L )." ) shouldBe "L = [9, 7, 6, 5, 4, 2]"
	}
}