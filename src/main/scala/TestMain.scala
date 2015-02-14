package ca.hyperreal.sprolog


object TestMain extends App
{
 	val p = Prolog.parseProgram( """
		d( X, X, 1 ).
		d( C, X, 0 ) :- number( C ).
		d( -A, X, -U ) :- d( A, X, U ).
		d( C*A, X, C*U ) :- number( C ), d( A, X, U ).
		d( X^N, X, N*X^(N - 1) ) :- integer( N ).
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
		op( A + C, C + A ) :- number( C ), !.
		op( A*C, C*A ) :- number( C ), !.
		op( C1*C2*A, C*A ) :- number( C1 ), number( C2 ), !, C is C1*C2.
		op( X, X ).
		
		dn( -(-(A)), B ) :- !, dn( A, B ).
		dn( -(A + B), U + V ) :- !, dn( -(A), U ), dn( -(B), V ).
		dn( -(A*B), U*V ) :- !, dn( -(A), U ), dn( -(B), V ).
		dn( A + B, U + V ) :- !, dn( A, U ), dn( B, V ).
		dn( A*B, U*V ) :- !, dn( A, U ), dn( B, V ).
		dn( A, A ).
		
		simp( X, Y ) :- dn( X, A ), s( A, Y ).
		""" )
	val db = Prolog.compileProgram( p )
	val v =
		new PrologVM
		{
		}
	
// 	Prolog.listing( db.code(Indicator('right, 3)).get )
	v.db = db
	v.ops = Prolog.ops
	
  	val q = Prolog.parseQuery( """ d( 3*x*x, x, X ), !, simp( X, Y ) """ )//iterate( [1,2,3], V )
	val qc = Prolog.compileQuery( q )

//  	println
//  	Prolog.listing( qc )
	
	v query qc
}
/*
		right( X, Y, L ) :- append( _, [X, Y|_], L ).

L = [a, b, c, d, e], X = b, Y = c, append( _, [X, Y|_], L )
	*/