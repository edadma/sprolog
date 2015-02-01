package ca.hyperreal.sprolog


trait AST

//case class AtomAST( atom: Symbol ) extends AST
case class IntAST( n: Int ) extends AST
case class NumberAST( n: Number ) extends AST
case class StringAST( s: String ) extends AST
case class VariableAST( v: Symbol ) extends AST
case class StructureAST( f: Symbol, args: IndexedSeq[AST] ) extends AST
{
	def arity = args.length
}
