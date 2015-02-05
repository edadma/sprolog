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
		pivoting( H, [], [], [] ).
		pivoting( H, [X|T], [X|L], G ) :- X >= H, pivoting( H, T, L, G ).
		pivoting( H, [X|T], L, [X|G] ) :- X < H, pivoting( H, T, L, G ).
		
		quick_sort( List, Sorted ) :- q_sort( List, [], Sorted ).
		
		q_sort( [], Acc, Acc ).
		q_sort( [H|T], Acc, Sorted ):-
			pivoting( H, T, L1, L2 ),
			q_sort( L1, Acc, Sorted1 ), q_sort( L2, [H|Sorted1], Sorted ).
		""" )
	val pc = Prolog.compileProgram( p )
	val v = Prolog.vm
	
// 	Prolog.listing( pc.code )
	v.program = pc

 	val q = Prolog.parseQuery( "quick_sort( [7, 4, 6, 5, 2, 9], L )." )
	val qc = Prolog.compileQuery( q )

//  	println
//  	Prolog.listing( qc.code )
	
	v query qc
}
