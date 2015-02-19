package ca.hyperreal.sprolog

import ca.hyperreal.rtcep.Position


abstract class AST
{
	var _pos: Position = null
	
	def pos( p: Position ) =
	{
		_pos = p
		this
	}
	
	def pos = _pos
}

case class AtomAST( atom: Symbol ) extends AST
case class NumberAST( n: Number ) extends AST
case class StringAST( s: String ) extends AST
case class ConstantAST( c: Any ) extends AST
case class VariableAST( v: Symbol ) extends AST
case class AnonymousAST() extends AST
case class StructureAST( f: Symbol, args: IndexedSeq[AST] ) extends AST
{
	def arity = args.length
}
