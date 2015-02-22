package ca.hyperreal.sprolog


object TestMain extends App
{
 	val p = Prolog.parseProgram( """
		a( G ) :- G.
		""" )
	val db = Prolog.compileProgram( p )
	val v = new WAM
	
//	Prolog.listing( db.code(Indicator('once, 1)).get )
	v.db = db
	v.ops = Prolog.ops
	v.predicates = Prolog.builtins
	
	val q = Prolog.parseQuery( """ traceon, call( write( hello ) ) """ )
	val qc = Prolog.compileQuery( q )

//  	println
//	Prolog.listing( qc )

	v query qc
}
