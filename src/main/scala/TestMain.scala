package ca.hyperreal.swam


object TestMain extends App
{
	val wam = new WAM
//	val p = Prolog.parse( "p( f(X), h(Y, f(a)), Y )." )._1.asInstanceOf[StructureAST]
//	val p = Prolog.parse( "p( Z, h(Z, W), f(W) )." )._1.asInstanceOf[StructureAST]
//	val p = Prolog.parse( "X = X." )._1.asInstanceOf[StructureAST]
	val p = Prolog.parseProgram( "p( a ). p( b ). X = X." )
	val pc = Prolog.program( p )

//	println( pc )
	wam.program = pc
	
//	val q = Prolog.parse( "p( Z, h(Z, W), f(W) )." )._1.asInstanceOf[StructureAST]
//	val q = Prolog.parse( "p( f(X), h(Y, f(a)), Y )." )._1.asInstanceOf[StructureAST]
//	val q = Prolog.parse( "p( f(X), h(Y, f(a)), Y ) = p( Z, h(Z, W), f(W) )." )._1.asInstanceOf[StructureAST]
// 	val q = Prolog.parseQuery( "q( X, Z ), r( Z, Y ), a = Z." )
	val q = Prolog.parseQuery( "p( A )." )
	val qc = Prolog.query( q )
	
//	println( qc )
	
	if (wam execute qc)
		println( "fail" )
	else
		println( wam.bindings )
}