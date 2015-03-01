package ca.hyperreal.sprolog


object TestMain extends App
{
 	val p = Prolog.parseProgram( """
/* squareroot(N, I) is true if I is the integer square root of the natural */
/*   number N.                                                             */
/*squareroot(N, I):-
  N >= 0,
  between(0, N, I),
  I * I =< N,
  (I + 1) * (I + 1) > N,
  !.

		between( I, J, I ) :- I =< J.
		between( I, J, K ) :- I < J, I1 is I + 1, between( I1, J, K ).
*/
		maplist( _, [], [] ).
		maplist( P, [X|L], [Y|M] ) :-
			Q =.. [P, X, Y], Q, maplist( P, L, M ).
			
		double( A, B ) :- B is 2*A.
		""" )
	val db = Prolog.compileProgram( p )
	val v = new WAM //{tracefile = "trace"}
	
//	Prolog.listing( db.code(Indicator('between, 3)).get )
	v.db = db
	v.ops = Prolog.ops
	v.predicates = Prolog.builtins
	
	val q = Prolog.parseQuery( """ maplist( double, [1, 2, 3], L ). """ )
	val qc = Prolog.compileQuery( q )

// 	println
// 	Prolog.listing( qc )

	v query qc
}

///**/squareroot( 9000000, R )