package ca.hyperreal.sprolog


object TestMain extends App
{
 	val p = Prolog.parseProgram( """
		F ; _ :- F.
		_ ; A :- A.
		
		repeat.
		repeat :- repeat.
		""" )
	val pc = Prolog.compileProgram( p )
	val v =
		new PrologVM
		{
		}
	
//	Prolog.listing( pc.code )
	v.program = pc

  	val q = Prolog.parseQuery( """ iterator_( [1, 2, 3], I ), repeat, next_( I, _ ), fail. """ )
	val qc = Prolog.compileQuery( q )

//  	println
//  	Prolog.listing( qc.code )
	
	v query qc
}
