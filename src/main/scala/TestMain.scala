package ca.hyperreal.sprolog


object TestMain extends App
{
 	val p = Prolog.parseProgram( """
		p( a ).
		p( b ).
		""" )
	val pc = Prolog.compileProgram( p )
	val v = new PrologVM
	
//	Prolog.listing( pc.code )
	v.program = pc

  	val q = Prolog.parseQuery( """ p( A ), !. """ )
	val qc = Prolog.compileQuery( q )

//  	println
//  	Prolog.listing( qc.code )
	
	v query qc
}
