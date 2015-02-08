package ca.hyperreal.sprolog

import funl.lia.{FunctionMap, Math}


class Evaluator
{
	def eval( e: AST ): Number =
		e match
		{
			case NumberAST( n, _ ) => n
			case VariableAST( _, pos ) => sys.error( "expressions should be ground" )
			case AtomAST( _, pos ) => sys.error( "expressions may not contain atoms" )
			case StringAST( _, pos ) => sys.error( "expressions may not contain strings" )
			case StructureAST( op@('+ |'- |'* |'/ ), Seq(left, right), _ ) => Math( op, eval(left), eval(right) ).asInstanceOf[Number]
			case StructureAST( 'min, Seq(left, right), _ ) =>
				val l = eval(left)
				val r = eval(right)
				
				if (Math( '<=, l, r ).asInstanceOf[Boolean])
					l
				else
					r
		}
}