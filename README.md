S-Prolog
========

This is the beginnings of a Warren Abstract Machine based Prolog interpreter in Scala.  Currently, it implements pure Prolog, that is Prolog with no cut (!) or other extra-logical predicates, none of the usual WAM optimizations (constant and list WAM instructions), and no tail recursion optimization.

It will aim to be a subset of ISO Prolog including a standard Prolog parser using https://github.com/edadma/rtcep.

Here is an example of what it can do so far.

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

output:

    L1 = [a, b, c, e], L2 = [a, c, d, e], M = a
    L1 = [a, b, c, e], L2 = [a, c, d, e], M = c
    L1 = [a, b, c, e], L2 = [a, c, d, e], M = e

    P = [a, b, c]
    P = [b, a, c]
    P = [b, c, a]
    P = [a, c, b]
    P = [c, a, b]
    P = [c, b, a]
