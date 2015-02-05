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
		naive_sort( List, Sorted ):- permutation( List, Sorted ), is_sorted( Sorted ).
		
		delete( X, [X|R], R ).
		delete( X, [F|R], [F|S] ) :- delete( X, R, S ).
		
		permutation( [], [] ).
		permutation( [X|Y], Z ) :- permutation( Y, W ), delete( X, Z, W ).   

		is_sorted( [] ).
		is_sorted( [_] ).
		is_sorted( [X, Y|T] ) :- X =< Y, is_sorted( [Y|T] ).
		""" )
	val pc = Prolog.compileProgram( p )

// 	Prolog.listing( pc.code )
	Prolog.vm.program = pc

 	val q = Prolog.parseQuery( "naive_sort( [7, 4, 6, 5, 2, 9], L )." )
//	val q = Prolog.parseQuery( "member( N, [7, 4, 6, 5, 2, 9, 3, 1, 8, 0] ), N =< 5." )
	val qc = Prolog.compileQuery( q )

//  	println
//  	Prolog.listing( qc.code )
	
	Prolog.vm query qc
}
