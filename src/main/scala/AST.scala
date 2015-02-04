package ca.hyperreal.sprolog

import ca.hyperreal.rtcep.Position


abstract class AST
{
	val pos: Position
}

//case class AtomAST( atom: Symbol, pos: Position = null ) extends AST
//case class IntAST( n: Int, pos: Position ) extends AST
case class NumberAST( n: Number, pos: Position = null ) extends AST
case class StringAST( s: String, pos: Position = null ) extends AST
case class VariableAST( v: Symbol, pos: Position = null ) extends AST
case class StructureAST( f: Symbol, args: IndexedSeq[AST], pos: Position = null ) extends AST
{
	def arity = args.length
}
