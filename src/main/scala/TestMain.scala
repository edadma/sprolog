package ca.hyperreal.sprolog


object TestMain extends App
{
 	val p = Prolog.parseProgram( """
		F ; _ :- F.
		_ ; A :- A.
		""" )
	val pc = Prolog.compileProgram( p )
	val v = new PrologVM
	
//	Prolog.listing( pc.code )
	v.program = pc

  	val q = Prolog.parseQuery( """ (X = 1; X = 2), call(!). """ )
	val qc = Prolog.compileQuery( q )

//  	println
//  	Prolog.listing( qc.code )
	
	v query qc
}
