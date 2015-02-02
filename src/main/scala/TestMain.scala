package ca.hyperreal.sprolog


object TestMain extends App
{
	val wam = new WAM
//	val p = Prolog.parse( "p( f(X), h(Y, f(a)), Y )." )._1.asInstanceOf[StructureAST]
//	val p = Prolog.parse( "p( Z, h(Z, W), f(W) )." )._1.asInstanceOf[StructureAST]
//	val p = Prolog.parse( "X = X." )._1.asInstanceOf[StructureAST]
// 	val p = Prolog.parseProgram( """
// X = X.
// 
// %man(adam).
// %man(peter).
// man(rick).
// man(paul).
// 
// %woman(marry).
// %woman(eve).
// 
// %parent(adam,peter). % means adam is parent of peter
// %parent(eve,peter).
// parent(rick,paul).
// %parent(marry,paul).
// 
// father(F,C) :- man(F), parent(F,C).
// mother(M,C) :- woman(M), parent(M,C).
// 
// """ )
//  	val p = Prolog.parseProgram( """
// p( a, b ).
// p( b, c ).
// p( X, Z ) :- p( X, Y ), p( Y, Z ).
// """ )
  	val p = Prolog.parseProgram( """
X = X.

%p( a ).
%p( b ).

q( a, u ).
%q( b, v ).
q( b, u ).

%r( X, Y ) :- q( X, Y ).
 """ )
	val pc = Prolog.program( p )

//	println( pc )
	wam.program = pc
	
//	val q = Prolog.parse( "p( Z, h(Z, W), f(W) )." )._1.asInstanceOf[StructureAST]
//	val q = Prolog.parse( "p( f(X), h(Y, f(a)), Y )." )._1.asInstanceOf[StructureAST]
//	val q = Prolog.parse( "p( f(X), h(Y, f(a)), Y ) = p( Z, h(Z, W), f(W) )." )._1.asInstanceOf[StructureAST]
// 	val q = Prolog.parseQuery( "q( X, Z ), r( Z, Y ), a = Z." )
// 	val q = Prolog.parseQuery( "father( A, B ), B = paul." )
	val q = Prolog.parseQuery( "q( A, B ), B = u." )//C = u  produces a different result!!!
	val qc = Prolog.query( q )
	
//	println( qc )
	
	if (wam execute qc)
		println( "no" )
	else
	{
		if (wam.bindings isEmpty)
			println( "yes" )
		else
			while (wam.alternative)
			{
				println( wam.bindings )
				wam.continue
			}
	}
}