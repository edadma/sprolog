package ca.hyperreal.sprolog


object TestMain extends App
{
//  	val p = Prolog.parseProgram( """
// 		X = X.
// 		
// 		man(adam).
// 		man(peter).
// 		man(rick).
// 		man(paul).
// 
// 		woman(marry).
// 		woman(eve).
// 
// 		parent(adam,peter). % means adam is parent of peter
// 		parent(eve,peter).
// 		parent(rick,paul).
// 		parent(marry,paul).
// 
// 		father(F,C) :- man(F), parent(F,C).
// 		mother(M,C) :- woman(M), parent(M,C).
// 		""" )
 	val p = Prolog.parseProgram( """
		p( f( b, g(a) ) ).
		""" )
	val pc = Prolog.compileProgram( p )

	Prolog.listing( pc.code )
	Prolog.vm.program = pc

  	val q = Prolog.parseQuery( "p( f( A, g(a) ) )." )
	val qc = Prolog.compileQuery( q )

 	println
 	Prolog.listing( qc.code )
	
	Prolog.vm query qc
}
