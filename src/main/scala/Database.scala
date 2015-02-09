package ca.hyperreal.sprolog

import collection.mutable.{ArrayBuffer, HashMap}


class Database
{
	private val procmap = new HashMap[Indicator, Procedure]
	private val program = new ArrayBuffer[Instruction]
	
	private case class Procedure( start: Int, length: Int, clauses: List[Clause] )
	
	def address( proc: Indicator ) = procmap.get( proc ) map {case Procedure(start, _, _) => start}
	
	def clauses( proc: Indicator ) = procmap.get( proc ) map {case Procedure(_, _, clauses) => clauses}
	
	def defined( proc: Indicator ) = procmap contains proc
	
	def instruction( address: Int ) = program(address)
	
	def procedure( proc: Indicator, clauses: List[Clause], code: Seq[Instruction] )
	{
		procmap(proc) = Procedure( program.size, code.length, clauses )
		
		program ++= code
	}
}

object Database
{
	val empty = new Database
}

case class Clause( head: AST, body: AST )
/*
		while_( C ) :- C.
		while_( C ) :- C, while_( C ).
		
		iterate( L, V ) :-
			iterator_( L, I ),
			while_( hasNext_(I) ),
				next_( I, V ).
*/