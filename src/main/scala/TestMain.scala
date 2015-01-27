package ca.hyperreal.swam


object TestMain extends App
{
	val wam = new WAM
 	val p = Prolog.parse( "p( f(X), h(Y, f(a)), Y )." )._1.asInstanceOf[StructureAST]
//	val p = Prolog.parse( "p( Z, h(Z, W), f(W) )." )._1.asInstanceOf[StructureAST]
//	val p = Prolog.parse( "X = X." )._1.asInstanceOf[StructureAST]
	val pc = Prolog.program( p )
	
		println( pc )
		wam.code = pc
		
 	val q = Prolog.parse( "p( Z, h(Z, W), f(W) )." )._1.asInstanceOf[StructureAST]
//	val q = Prolog.parse( "p( f(X), h(Y, f(a)), Y )." )._1.asInstanceOf[StructureAST]
//	val q = Prolog.parse( "A = asdf." )._1.asInstanceOf[StructureAST]
	val (qc, vars) = Prolog.query( q )

// 	println( qc )
// 	println( vars )
	
	if (wam execute qc)
		println( "fail" )
	else
		println( wam.bindings(vars) )
// 		
// 	println( wam.heap )
}