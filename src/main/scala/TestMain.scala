package ca.hyperreal.sprolog


object TestMain extends App
{
 	val p = Prolog.parseProgram( """
		length( Xs, L ) :- length_( Xs, 0, L ) .

		length_( []     , L , L ) .
		length_( [_|Xs] , T , L ) :-
			T1 is T + 1,
			length_( Xs, T1, L ).
		""" )
	val db = Prolog.compileProgram( p )
	val v =
		new PrologVM
		{
		}
	
//	Prolog.listing( pc.code )
	v.db = db

  	val q = Prolog.parseQuery( """ length( [4, 5, 6], L ). """ )//iterate( [1,2,3], V )
	val qc = Prolog.compileQuery( q )

//  	println
//  	Prolog.listing( qc.code )
	
	v query qc
}
