package ca.hyperreal.sprolog


object TestMain extends App
{
 	val p = Prolog.parseProgram( """
		right( X, Y, L ) :- append( _, [X, Y|_], L ).
		""" )
	val db = Prolog.compileProgram( p )
	val v =
		new PrologVM
		{
		}
	
	Prolog.listing( db.code(Indicator('right, 3)).get )
	v.db = db

  	val q = Prolog.parseQuery( """ L = [a, b, c, d, e], X = b, Y = c, append( _, [X, Y|_], L ) """ )//iterate( [1,2,3], V )
	val qc = Prolog.compileQuery( q )

 	println
 	Prolog.listing( qc )
	
	v query qc
}
/*
		right( X, Y, L ) :- append( _, [X, Y|_], L ).

L = [a, b, c, d, e], X = b, Y = c, append( _, [X, Y|_], L )
	*/