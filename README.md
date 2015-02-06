S-Prolog
========

This is a Warren Abstract Machine based Prolog interpreter in Scala.  Currently, it implements pure Prolog, that is Prolog with no cut (!) and no tail recursion optimization.  Many of the WAM optimization instructions are not yet implemented, however the ones supporting constants and anonymous variables are.

It will aim to be a subset of ISO Prolog including a standard Prolog parser using https://github.com/edadma/rtcep.

Here is an example of what it can do so far.

    val p = Prolog.program( """
        member( X, [X|_] ).
        member( X, [_|T] ) :- member( X, T ).
        
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

output:

    L1 = [a, b, c, e], L2 = [a, c, d, e], M = a
    L1 = [a, b, c, e], L2 = [a, c, d, e], M = c
    L1 = [a, b, c, e], L2 = [a, c, d, e], M = e

    L = [2, 4, 5, 6, 7, 9]