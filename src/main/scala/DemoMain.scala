package xyz.hyperreal.sprolog


object DemoMain extends App
{
	val p = Prolog.program( """
		/*
		 * Symbolic differentiation and simplification
		 */
		d( X, X, 1 ).
		d( C, X, 0 ) :- number( C ).
		d( -A, X, -U ) :- d( A, X, U ).
		d( C*A, X, C*U ) :- number( C ), d( A, X, U ).
		d( A + B, X, U + V ) :- d( A, X, U ), d( B, X, V ).
		d( A - B, X, U - V ) :- d( A, X, U ), d( B, X, V ).
		d( A*B, X, B*U + A*V ) :- d( A, X, U ), d( B, X, V ).
		
		s( A + B, C ) :- !, s( A, A1 ), s( B, B1 ), op( A1 + B1, C ).
		s( A - B, C ) :- !, s( A, A1 ), s( B, B1 ), op( A1 - B1, C ).
		s( A*B, C ) :- !, s( A, A1 ), s( B, B1 ), op( A1*B1, C ).
		s( X, X ).
		
		op( A + B, C ) :- number( A ), number( B ), !, C is A + B.
		op( 0 + A, A ) :- !.
		op( A + 0, A ) :- !.
		op( 1*A, A ) :- !.
		op( 0*A, 0 ) :- !.
		op( A*1, A ) :- !.
		op( A*0, 0 ) :- !.
		op( A - 0, A ) :- !.
		op( A - A, 0 ) :- !.
		op( A + A, 2*A ) :- !.
		op( X, X ).
		
		dn( -(-(A)), B ) :- !, dn( A, B ).
		dn( -(A + B), U + V ) :- !, dn( -(A), U ), dn( -(B), V ).
		dn( -(A*B), U*V ) :- !, dn( -(A), U ), dn( -(B), V ).
		dn( A + B, U + V ) :- !, dn( A, U ), dn( B, V ).
		dn( A*B, U*V ) :- !, dn( A, U ), dn( B, V ).
		dn( A, A ).
		
		simp( X, Y ) :- dn( X, A ), s( A, Y ).
		
		/*
		 * Quicksort
		 */
		pivoting( H, [], [], [] ).
		pivoting( H, [X|T], [X|L], G ) :- X >= H, pivoting( H, T, L, G ).
		pivoting( H, [X|T], L, [X|G] ) :- X < H, pivoting( H, T, L, G ).
		
		quick_sort( List, Sorted ) :- q_sort( List, [], Sorted ).
		
		q_sort( [], Acc, Acc ).
		q_sort( [H|T], Acc, Sorted ):-
			pivoting( H, T, L1, L2 ),
			q_sort( L1, Acc, Sorted1 ), q_sort( L2, [H|Sorted1], Sorted ).
        """ )

    println( Prolog.query(p, "d( x*x - 2, x, X ), simp( X, Y ).") )
    println
    println( Prolog.query(p, "quick_sort( [7, 4, 6, 5, 2, 9], L ).") )
}