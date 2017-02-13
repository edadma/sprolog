package xyz.hyperreal.sprolog

import xyz.hyperreal.lia.{FunctionMap, Math}


class Evaluator
{
	def eval( e: AST ): Number =
		e match
		{
			case NumberAST( n ) => n
			case VariableAST( _ ) => sys.error( "expressions should be ground" )
			case AtomAST( _ ) => sys.error( "expressions may not contain atoms" )
			case StringAST( _ ) => sys.error( "expressions may not contain strings" )
			case StructureAST( op@('+ |'- |'* |'/ |'mod ), Seq(left, right) ) => Math( op, eval(left), eval(right) ).asInstanceOf[Number]
			case StructureAST( 'min, Seq(left, right) ) =>
				val l = eval(left)
				val r = eval(right)
				
				if (Math( '<=, l, r ).asInstanceOf[Boolean])
					l
				else
					r
		}
}