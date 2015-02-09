package ca.hyperreal.sprolog


object DemoMain extends App
{
	val p = Prolog.program( """
		pivoting( H, [], [], [] ).
		pivoting( H, [X|T], [X|L], G ) :- X >= H, pivoting( H, T, L, G ).
		pivoting( H, [X|T], L, [X|G] ) :- X < H, pivoting( H, T, L, G ).
		
		quick_sort( List, Sorted ) :- q_sort( List, [], Sorted ).
		
		q_sort( [], Acc, Acc ).
		q_sort( [H|T], Acc, Sorted ):-
			pivoting( H, T, L1, L2 ),
			q_sort( L1, Acc, Sorted1 ), q_sort( L2, [H|Sorted1], Sorted ).
        """ )

    println( Prolog.query(p, "L1 = [a, b, c, e], L2 = [a, c, d, e], member( M, L1 ), member( M, L2 ).") )
    println
    println( Prolog.query(p, "quick_sort( [7, 4, 6, 5, 2, 9], L ).") )
}