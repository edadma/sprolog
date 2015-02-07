package ca.hyperreal.sprolog


object TestMain extends App
{
 	val p = Prolog.parseProgram( """
		""" )
	val pc = Prolog.compileProgram( p )
	val v = new PrologVM
	
//	Prolog.listing( pc.code )
	v.program = pc

  	val q = Prolog.parseQuery( """ A = hello, call( (write(A), write(f(A))) ). """ )
	val qc = Prolog.compileQuery( q )

//  	println
//  	Prolog.listing( qc.code )
	
	v query qc
}
