package ca.hyperreal.sprolog


object TestMain extends App
{
 	val p = Prolog.parseProgram( """
		insect( bee ).
		insect( ant ) :- !.
		insect( beetle ).
		""" )
	val db = Prolog.compileProgram( p )
	val v = new WAM
	
  	Prolog.listing( db.code(Indicator('insect, 1)).get )
	v.db = db
	v.ops = Prolog.ops
	v.predicates = Prolog.builtins
	
	val q = Prolog.parseQuery( """ insect(X) """ )
	val qc = Prolog.compileQuery( q )

//  	println
//  	Prolog.listing( qc )

	v query qc
}
