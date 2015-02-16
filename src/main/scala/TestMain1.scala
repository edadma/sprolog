package ca.hyperreal.sprolog

import Prolog._


object TestMain1 extends App
{
	val p = new PrologDB
	
	query( p, "true ; true." )
	println( query( p, "(X = 1; X = 2), call(!)." ) )
}
