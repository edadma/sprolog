S-Prolog
========

S-Prolog is a Warren Abstract Machine (WAM) based Prolog interpreter in Scala.  Currently, a large subset of the WAM instruction set is implemented.  There is no tail recursion optimization or procedure indexing for fast rule matching yet.  The specialized optimizing instructions supporting constants, lists and anonymous variables are implemented.  Most of the commonly used built-in predicates are implemented.

This will aim to be a subset of ISO Prolog including a standard Prolog parser using https://github.com/edadma/rtcep.

Here is an example of what it can do so far.

    val p = Prolog.program( """
		/*
		 * Symbolic differentiation and simplification
		 */
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
		
		/*
		 * Quicksort
		 */
		pivoting( H, [], [], [] ).
		pivoting( H, [X|T], [X|L], G ) :- X >= H, pivoting( H, T, L, G ).
		pivoting( H, [X|T], L, [X|G] ) :- X < H, pivoting( H, T, L, G ).
		
		quick_sort( List, Sorted ) :- q_sort( List, [], Sorted ).
		
		q_sort( [], Acc, Acc ).
		q_sort( [H|T], Acc, Sorted ):-
			pivoting( H, T, L1, L2 ),
			q_sort( L1, Acc, Sorted1 ), q_sort( L2, [H|Sorted1], Sorted ).
        """ )

    println( Prolog.query(p, "d( x*x - 2, x, X ), simp( X, Y ).") )
    println
    println( Prolog.query(p, "quick_sort( [7, 4, 6, 5, 2, 9], L ).") )

output:

    X = x*1 + x*1 - 0, Y = 2*x

    L = [2, 4, 5, 6, 7, 9]
    

## License

S-Prolog is distributed under the MIT License, meaning that you are free to use it in your free or proprietary software.


## Usage

Use the following elements to use S-Prolog in your Maven project:

	<repository>
		<id>hyperreal</id>
		<url>https://dl.bintray.com/edadma/maven</url>
	</repository>

	<dependency>
		<groupId>ca.hyperreal</groupId>
		<artifactId>sprolog</artifactId>
		<version>0.1</version>
	</dependency>

Add the following to your `build.sbt` file to use S-Prolog in your SBT project:

	resolvers += "Hyperreal Repository" at "https://dl.bintray.com/edadma/maven"

	libraryDependencies += "ca.hyperreal" %% "sprolog" % "0.1"
