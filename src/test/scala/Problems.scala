package ca.hyperreal.sprolog

import org.scalatest._
import prop.PropertyChecks

import Prolog.{program, query, queryOnce}


class Problems extends FreeSpec with PropertyChecks with Matchers
{
	"zebra" in
	{
	val p = program( """
		next( X, Y, L ) :- right( X, Y, L ) ; right( Y, X, L ).

		right( X, Y, L ) :- append( _, [X, Y|_], L ).

		zebra( Zebra, Water ) :-
			length( L, 5 ), !, 											%  1
			member( [red, english, _, _, _], L ),							%  2
			member( [_, spanish, dog, _, _], L ),							%  3
			member( [green, _, _, coffee, _] , L ),						%  4
			member( [_, ukrainian, _, tea, _] , L ),						%  5
			right( [ivory, _, _, _, _], [green, _, _, _, _], L ),			%  6
			member( [_, _, snails, _, old_gold], L ),						%  7
			member( [yellow, _, _, _, kool], L ),							%  8
			L = [_, _, [_, _, _, milk, _], _, _],							%  9
			L = [[_, norwegian, _, _, _]|_],								% 10
			next( [_, _, _, _, chesterfields], [_, _, fox, _, _], L ),		% 11
			next( [_, _, _, _, kool], [_, _, horse, _, _], L ),			% 12
			member( [_, _, _, orange, lucky_strike], L ),					% 13
			member( [_, japanese, _, _, parliaments], L ),				% 14
			next( [_, norwegian, _, _, _], [blue, _, _, _, _], L ),		% 15
			member( [_, Zebra, zebra, _, _], L ),							% Q1
			member( [_, Water, _, water, _], L ).							% Q2
		""" )
	
		query( p, "zebra( Zebra, Water )." ) shouldBe "Water = norwegian, Zebra = japanese"
	}
	
	"sudoku" in
	{
	val p = program( """
		sudoku( Cells ):- 
			Cells = [[A1, A2, A3, A4, A5, A6, A7, A8, A9], 
					[B1, B2, B3, B4, B5, B6, B7, B8, B9], 
					[C1, C2, C3, C4, C5, C6, C7, C8, C9], 
					[D1, D2, D3, D4, D5, D6, D7, D8, D9], 
					[E1, E2, E3, E4, E5, E6, E7, E8, E9], 
					[F1, F2, F3, F4, F5, F6, F7, F8, F9], 
					[G1, G2, G3, G4, G5, G6, G7, G8, G9], 
					[H1, H2, H3, H4, H5, H6, H7, H8, H9], 
					[I1, I2, I3, I4, I5, I6, I7, I8, I9]], 
				 
		value(A1), value(A2), value(A3), value(A4), value(A5), value(A6), value(A7), value(A8), value(A9), 
		distinct([ A1, A2, A3, A4, A5, A6, A7, A8, A9 ]), 
		value(B1), value(B2), value(B3), value(B4), value(B5), value(B6), value(B7), value(B8), value(B9), 
		distinct([ B1, B2, B3, B4, B5, B6, B7, B8, B9 ]), 
		value(C1), value(C2), value(C3), 
		distinct([ C1, C2, C3 ]), 
		distinct([ A1, A2, A3, B1, B2, B3, C1, C2, C3 ]), 
		value(C4), value(C5), value(C6), 
		distinct([ C1, C2, C3, C4, C5, C6 ]), 
		distinct([ A4, A5, A6, B4, B5, B6, C4, C5, C6 ]),
		value(C7), value(C8), value(C9),
		distinct([ C1, C2, C3, C4, C5, C6, C7, C8, C9 ]), 
		distinct([ A7, A8, A9, B7, B8, B9, C7, C8, C9 ]),
		value(D1), value(D2), value(D3), value(D4), value(D5), value(D6), value(D7), value(D8), value(D9), 
		distinct([ D1, D2, D3, D4, D5, D6, D7, D8, D9 ]), 
		value(E1), value(E2), value(E3), value(E4), value(E5), value(E6), value(E7), value(E8), value(E9), 
		distinct([ E1, E2, E3, E4, E5, E6, E7, E8, E9 ]), 
		value(F1), value(F2), value(F3),  
		distinct([ F1, F2, F3 ]), 
		distinct([ D1, D2, D3, E1, E2, E3, F1, F2, F3 ]), 
		value(F4), value(F5), value(F6), 
		distinct([ F1, F2, F3, F4, F5, F6 ]), 
		distinct([ D4, D5, D6, E4, E5, E6, F4, F5, F6 ]),
		value(F7), value(F8), value(F9),
		distinct([ F1, F2, F3, F4, F5, F6, F7, F8, F9 ]), 
		distinct([ D7, D8, D9, E7, E8, E9, F7, F8, F9 ]), 
		value(G1), value(G2), value(G3), value(G4), value(G5), value(G6), value(G7), value(G8), value(G9), 
		distinct([ G1, G2, G3, G4, G5, G6, G7, G8, G9 ]), 
		value(H1), value(H2), value(H3), value(H4), value(H5), value(H6), value(H7), value(H8), value(H9), 
		distinct([ H1, H2, H3, H4, H5, H6, H7, H8, H9 ]), 
		value(I1), value(I2), value(I3),  
		distinct([ I1, I2, I3 ]), 
		distinct([ G1, G2, G3, H1, H2, H3, I1, I2, I3 ]),
		value(I4), value(I5), value(I6),
		distinct([ I1, I2, I3, I4, I5, I6 ]), 
		distinct([ G4, G5, G6, H4, H5, H6, I4, I5, I6 ]),
		value(I7), value(I8), value(I9),
		distinct([ I1, I2, I3, I4, I5, I6, I7, I8, I9 ]), 
		distinct([ G7, G8, G9, H7, H8, H9, I7, I8, I9 ]),

		% Ensure all Columns have distinct values 
		distinct([ A1, B1, C1, D1, E1, F1, G1, H1, I1 ]), 
		distinct([ A2, B2, C2, D2, E2, F2, G2, H2, I2 ]), 
		distinct([ A3, B3, C3, D3, E3, F3, G3, H3, I3 ]), 
		distinct([ A4, B4, C4, D4, E4, F4, G4, H4, I4 ]), 
		distinct([ A5, B5, C5, D5, E5, F5, G5, H5, I5 ]), 
		distinct([ A6, B6, C6, D6, E6, F6, G6, H6, I6 ]), 
		distinct([ A7, B7, C7, D7, E7, F7, G7, H7, I7 ]), 
		distinct([ A8, B8, C8, D8, E8, F8, G8, H8, I8 ]), 
		distinct([ A9, B9, C9, D9, E9, F9, G9, H9, I9 ]). 

		value(1). 
		value(2). 
		value(3). 
		value(4). 
		value(5). 
		value(6). 
		value(7). 
		value(8). 
		value(9). 

		distinct([]).
		distinct([X|Xs]) :- 
			different(Xs,X), 
			distinct(Xs).

		different([],_).
		different([Y|Ys],X) :- 
			X \= Y, 
			different(Ys,X).
		""" )
	
		query( p, """Puzzle = [
			[_,4,8, _,3,6, _,2,1],
			[3,1,7, 2,5,8, 6,9,4],
			[9,6,_, 4,7,_, 5,3,_],
			
			[2,7,6, 8,1,5, 3,4,9],
			[_,8,3, _,2,9, _,5,7],
			[1,_,5, 7,_,3, 2,_,6],
			
			[_,3,4, _,6,2, _,7,5],
			[7,5,1, 3,9,4, 8,6,2],
			[6,2,9, 5,8,7, 4,1,3]], sudoku( Puzzle )""" ) shouldBe "Puzzle = [[5, 4, 8, 9, 3, 6, 7, 2, 1], [3, 1, 7, 2, 5, 8, 6, 9, 4], [9, 6, 2, 4, 7, 1, 5, 3, 8], [2, 7, 6, 8, 1, 5, 3, 4, 9], [4, 8, 3, 6, 2, 9, 1, 5, 7], [1, 9, 5, 7, 4, 3, 2, 8, 6], [8, 3, 4, 1, 6, 2, 9, 7, 5], [7, 5, 1, 3, 9, 4, 8, 6, 2], [6, 2, 9, 5, 8, 7, 4, 1, 3]]"
	}
	
	"symbolic differentiation" in
	{
	val p = program( """
		d( X, X, 1 ).
		d( C, X, 0 ) :- number( C ).
		d( -A, X, -U ) :- d( A, X, U ).
		d( C*A, X, C*U ) :- number( C ), d( A, X, U ).
		d( A + B, X, U + V ) :- d( A, X, U ), d( B, X, V ).
		d( A - B, X, U - V ) :- d( A, X, U ), d( B, X, V ).
		d( A*B, X, B*U + A*V ) :- d( A, X, U ), d( B, X, V ).
		
		s( A + B, C ) :- !, s( A, A1 ), s( B, B1 ), op( A1 + B1, C ).
		s( A - B, C ) :- !, s( A, A1 ), s( B, B1 ), op( A1 - B1, C ).
		s( A*B, C ) :- !, s( A, A1 ), s( B, B1 ), op( A1*B1, C ).
		s( X, X ).
		
		op( A + B, C ) :- number( A ), number( B ), !, C is A + B.
		op( 0 + A, A ) :- !.
		op( A + 0, A ) :- !.
		op( 1*A, A ) :- !.
		op( 0*A, 0 ) :- !.
		op( A*1, A ) :- !.
		op( A*0, 0 ) :- !.
		op( A - 0, A ) :- !.
		op( A - A, 0 ) :- !.
		op( A + A, 2*A ) :- !.
		op( X, X ).
		
		dn( -(-(A)), B ) :- !, dn( A, B ).
		dn( -(A + B), U + V ) :- !, dn( -(A), U ), dn( -(B), V ).
		dn( -(A*B), U*V ) :- !, dn( -(A), U ), dn( -(B), V ).
		dn( A + B, U + V ) :- !, dn( A, U ), dn( B, V ).
		dn( A*B, U*V ) :- !, dn( A, U ), dn( B, V ).
		dn( A, A ).
		
		simp( X, Y ) :- dn( X, A ), s( A, Y ).
		""" )
	
		query( p, "d( x*x - 2, x, X ), simp( X, Y )" ) shouldBe "X = x*1 + x*1 - 0, Y = 2*x"
	}
}