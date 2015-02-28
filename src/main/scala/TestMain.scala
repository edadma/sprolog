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
*/
		between( I, J, I ) :- I =< J.
		between( I, J, K ) :- I < J, I1 is I + 1, between( I1, J, K ).

		ackermann( 0, N, Ans ) :- !, Ans is N + 1.
		ackermann( M, 0, Ans ) :- M > 0, !, X is M - 1, ackermann( X, 1, Ans ).
		ackermann( M, N, Ans ) :- M > 0, N > 0, X is M - 1, Y is N - 1, ackermann( M, Y, Ans2 ), ackermann( X, Ans2, Ans ).
""" )
	val db = Prolog.compileProgram( p )
	val v = new WAM //{tracefile = "trace"}
	
//	Prolog.listing( db.code(Indicator('between, 3)).get )
	v.db = db
	v.ops = Prolog.ops
	v.predicates = Prolog.builtins
	
	val q = Prolog.parseQuery( """ between( 0, 3, M ), between( 0, 6, N ), ackermann( M, N, Result ). """ )
	val qc = Prolog.compileQuery( q )

// 	println
// 	Prolog.listing( qc )

	v query qc
}

///**/squareroot( 9000000, R )