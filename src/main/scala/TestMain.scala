package ca.hyperreal.sprolog


object TestMain extends App
{
	val wam = new WAM
// 	val p = Prolog.parseProgram( """
// X = X.
// 
// man(adam).
// man(peter).
// man(rick).
// man(paul).
// 
// woman(marry).
// woman(eve).
// 
// parent(adam,peter). % means adam is parent of peter
// parent(eve,peter).
// parent(rick,paul).
// parent(marry,paul).
// 
// father(F,C) :- man(F), parent(F,C).
// mother(M,C) :- woman(M), parent(M,C).
// 
// """ )
	val p = Prolog.parseProgram( """
%X = X.

%member( X, [X|_] ).
%member( X, [_|T] ) :- member( X, T ).

reverse(List, Reversed) :-
          reverse(List, [], Reversed).

reverse([], Reversed, Reversed).
reverse([Head|Tail], SoFar, Reversed) :-
          reverse(Tail, [Head|SoFar], Reversed).

%append([],L,L).
%append([H|T],L,[H|LT]):-append(T,L,LT).
""" )
	val pc = Prolog.compileProgram( p )

//	println( pc )
	wam.program = pc
	
//  	val q = Prolog.parseQuery( "father( A, B ), B = paul." )
 	val q = Prolog.parseQuery( "reverse( [a, b, c], L )." )
	val qc = Prolog.compileQuery( q )
	
//	println( qc )
	
	Prolog.execute( wam, qc )
}