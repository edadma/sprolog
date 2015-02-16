package ca.hyperreal.sprolog


object TestMain extends App
{
 	val p = Prolog.parseProgram( """
		""" )
	val db = Prolog.compileProgram( p )
	val v =
		new PrologVM
		{
		}
	
//  	Prolog.listing( db.code(Indicator('right, 3)).get )
	v.db = db
	v.ops = Prolog.ops
	
	val q = Prolog.parseQuery( """  """ )
	val qc = Prolog.compileQuery( q )

//  	println
//  	Prolog.listing( qc )

	v query qc
}
