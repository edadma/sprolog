package xyz.hyperreal.sprolog


class PrologDB extends Database
{
	Prolog.compileProgram( Prolog.parseProgram("""
%		X = X.	% for some reason this doesn't work

		If -> Then        :- If, !, Then.
		If -> Then ; _    :- If, !, Then.
		_  -> _    ; Else :- !, Else.		% the cut stops the rules for disjunction (;) from being tried
		
		F ; _ :- F.
		_ ; A :- A.
		
		\+ Goal :- Goal, !, fail.
		\+ _.

		once( Goal ) :- Goal, !.
		
		repeat.
		repeat :- repeat.
		
		sublist( [], L ).
		sublist( [H|T], [H|U] ) :- sublist_( T, U ).
		sublist( S, [H|U] ) :- sublist( S, U ).

			sublist_( [], L ).
			sublist_( [H|T], [H|U] ) :- sublist_( T, U ). 

		member( X, [X|_] ).
		member( X, [_|T] ) :- member( X, T ).
		
		append( [], L, L ).
		append( [H|T], L, [H|LT] ) :- append( T, L, LT ).
		
		reverse( List, Reversed ) :- reverse_( List, [], Reversed ).

			reverse_( [], Reversed, Reversed ).
			reverse_( [Head|Tail], SoFar, Reversed ) :- reverse_( Tail, [Head|SoFar], Reversed ).
			
		length( Xs, L ) :- length_( Xs, 0, L ) .

			length_( []     , L , L ) .
			length_( [_|Xs] , T , L ) :-
				T1 is T + 1,
				length_( Xs, T1, L ).
			
		sum_list( L, R ) :-
			sum_list_( L, 0, R ).

			sum_list_( [], F, F ).
			sum_list_( [H|T], F, R ) :-
				F2 is F + H,
				sum_list_( T, F2 ,R ).
		
		delete( X, [X|R], R ).
		delete( X, [F|R], [F|S] ) :- delete( X, R, S ).
		
		permutation( [], [] ).
		permutation( [X|Y], Z ) :- permutation( Y, W ), delete( X, Z, W ).   
		
		halve( L, A, B ) :- halve_( L, L, A, B ).
		
			halve_( [], R, [], R ).   % for lists of even length
			halve_( [_], R, [], R ).  % for lists of odd length
			halve_( [_, _|T], [X|L], [X|L1], R ) :- halve_( T, L, L1, R ).
		
		powerset(Set, PowerSet):-
			powerset_(Set, [[]], PowerSet).
		
			powerset_([], Yss, Yss).
			powerset_([X|Xs], Yss0, Yss):-
				powerset__(Yss0, X, Yss1),
				powerset_(Xs, Yss1, Yss).

				powerset__([], _, []).
				powerset__([Zs|Zss], X, [Zs, [X|Zs]|Yss]):-
					powerset__(Zss, X, Yss).
		
		subset( [], _ ).
		subset( [H|T], L ) :-
			member( H, L ),
			subset( T, L ).

		union( [], X, X ) :- !.
		union( [X|R], Y, Z ) :- member( X, Y ), union( R, Y, Z ), !.
		union( [X|R], Y, [X|Z] ) :- union( R, Y, Z ).

		intersection( [], _, [] ) :- !.
		intersection( [X|R], Y, [X|T] ) :- member( X, Y ), intersection( R, Y, T ), !.
		intersection( [X|R], Y, L ) :- intersection( R, Y, L ).
			
		while_( C ) :- C.
		while_( C ) :- C, while_( C ).
		
		iterate_( L, V ) :-
			iterator_( L, I ),
			while_( hasNext_(I) ),
				next_( I, V ).
		"""), this )
}
