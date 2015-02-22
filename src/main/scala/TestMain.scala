package ca.hyperreal.sprolog


object TestMain extends App
{
 	val p = Prolog.parseProgram( """
		""" )
	val db = Prolog.compileProgram( p )
	val v = new WAM
	
//	Prolog.listing( db.code(Indicator('once, 1)).get )
	v.db = db
	v.ops = Prolog.ops
	v.predicates = Prolog.builtins
	
	val q = Prolog.parseQuery( """ (X = 1 ; X = 2), write(X) """ )
	val qc = Prolog.compileQuery( q )

//  	println
//	Prolog.listing( qc )

	v query qc
}
