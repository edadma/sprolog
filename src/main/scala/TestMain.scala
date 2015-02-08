package ca.hyperreal.sprolog


object TestMain extends App
{
 	val p = Prolog.parseProgram( """
		\+ Goal :- Goal, !, fail.
		\+ _.
		
		If -> Then        :- If, !, Then.
		If -> Then ; _    :- If, !, Then.
		_  -> _    ; Else :- !, Else.		% the cut stops the rules for disjunction (;) from being tried
		
		repeat.
		repeat :- repeat.
		
		while_( C ) :- C.
		while_( C ) :- C, while_( C ).
		
		iterate( L, V ) :-
			iterator_( L, I ),
			while_( hasNext_(I) ),
				next_( I, V ).
		""" )
	val db = Prolog.compileProgram( p )
	val v =
		new PrologVM
		{
		}
	
//	Prolog.listing( pc.code )
	v.db = db

  	val q = Prolog.parseQuery( """ iterate( [1,2,3], V ). """ )
	val qc = Prolog.compileQuery( q )

//  	println
//  	Prolog.listing( qc.code )
	
	v query qc
}
