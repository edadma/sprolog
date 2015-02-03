package ca.hyperreal.sprolog


object DemoMain extends App
{
	val p = Prolog.program( """
		X = X.
		
		member( X, [X|_] ).
		member( X, [_|T] ) :- member( X, T ).
		
		delete( X, [X|R], R ).
		delete( X, [F|R], [F|S] ) :- delete( X, R, S ).
		
		permutation( [], [] ).
		permutation( [X|Y], Z ) :- permutation( Y, W ), delete( X, Z, W ).   
		""" )
	
	println( Prolog.query(p, "L1 = [a, b, c, e], L2 = [a, c, d, e], member( M, L1 ), member( M, L2 ).") )
	println
	println( Prolog.query(p, "permutation( [a, b, c], P ).") )
}