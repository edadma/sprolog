package ca.hyperreal.sprolog


object TestMain extends App
{
 	val p = Prolog.parseProgram( """
		union( [], X, X ) :- !.
		union( [X|R], Y, Z ) :- member( X, Y ), union( R, Y, Z ), !.
		union( [X|R], Y, [X|Z] ) :- union( R, Y, Z ).
		""" )
	val db = Prolog.compileProgram( p )
	val v =
		new PrologVM
		{
		}
	
//	Prolog.listing( pc.code )
	v.db = db

  	val q = Prolog.parseQuery( """ union( [a, b, c], [c, b, d], R ). """ )//iterate( [1,2,3], V )
	val qc = Prolog.compileQuery( q )

//  	println
//  	Prolog.listing( qc.code )
	
	v query qc
}
