package ca.hyperreal.swam

import collection.mutable.HashSet


object TestMain extends App
{
	val wam = new WAM
//	val p = Prolog.parse( "p( f(X), h(Y, f(a)), Y )." )._1.asInstanceOf[StructureAST]
//	val p = Prolog.parse( "p( Z, h(Z, W), f(W) )." )._1.asInstanceOf[StructureAST]
//	val p = Prolog.parse( "X = X." )._1.asInstanceOf[StructureAST]
	val p = Prolog.parse( "q( A, f(A) )." )._1.asInstanceOf[StructureAST]// r( A, f(A) ).
	val pc = Prolog.program( p )

//	println( pc )
	wam.program = pc
	
//	val q = Prolog.parse( "p( Z, h(Z, W), f(W) )." )._1.asInstanceOf[StructureAST]
//	val q = Prolog.parse( "p( f(X), h(Y, f(a)), Y )." )._1.asInstanceOf[StructureAST]
//	val q = Prolog.parse( "p( f(X), h(Y, f(a)), Y ) = p( Z, h(Z, W), f(W) )." )._1.asInstanceOf[StructureAST]
	val q = Prolog.parse( "q( X, Z ), q( Z, Y )." )._1.asInstanceOf[StructureAST]
	val qc = Prolog.query( q )
	
//	println( qc )
	
	if (wam execute qc)
		println( "fail" )
	else
		println( wam.bindings )
}