package ca.hyperreal.sprolog


object TestMain extends App
{
 	val p = Prolog.parseProgram( """
		p( X, Y ) :- X = q([a, b], c), Y = a.
		""" )
	val db = Prolog.compileProgram( p )
	val v = new WAM
	
//	Prolog.listing( db.code(Indicator('peek, 2)).get )
	v.db = db
	v.ops = Prolog.ops
	v.predicates = Prolog.builtins
	
	val q = Prolog.parseQuery( """ p( q([X|_], _), X ) """ )
	val qc = Prolog.compileQuery( q )

//  	println
  	Prolog.listing( qc )

	v query qc
}
// 		hamming :-
// 			init_queue(3, R),
// 			write( R ), nl,
// 			peek(R, B),
// 			write( B ), nl.
// 	
// 		init_queue(P, q([P|Zs],Zs)).
// 
// 		enqueue(X, q(Ys,[X|Zs]), q(Ys,Zs)).
// 
// 		dequeue(X, q([X|Ys],Zs), q(Ys,Zs)).
// 
// 		peek(q([X|_],_), X).
