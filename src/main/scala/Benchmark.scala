package xyz.hyperreal.sprolog

import Prolog._


object Benchmark extends App
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
	
	compat.Platform.collectGarbage
	compat.Platform.collectGarbage
	val start = compat.Platform.currentTime
	
	println( query( p, """Puzzle = [
		[_,4,_, _,3,_, _,2,_],
		[_,_,7, 2,_,8, 6,9,_],
		[9,6,_, 4,7,_, 5,3,_],
		
		[_,7,6, 8,_,5, 3,4,_],
		[_,8,3, _,2,9, _,5,7],
		[1,_,5, 7,_,3, 2,_,6],
		
		[_,3,4, _,6,2, _,7,5],
		[_,5,1, 3,_,4, 8,6,_],
		[6,_,9, 5,8,_, 4,_,3]], sudoku( Puzzle )""" ) )
	
	printf( "%.1f seconds\n", (compat.Platform.currentTime - start)/1000.0 )
}
