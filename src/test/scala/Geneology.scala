package ca.hyperreal.sprolog

import org.scalatest._
import prop.PropertyChecks

import Prolog.{program, query, queryOnce}


class Genealogy extends FreeSpec with PropertyChecks with Matchers
{
	"tree" in
	{
	val p = program( """
		parent(albertus, ralph).
		parent(albert, alberta).
		parent(ralph, neal).
		parent(ralph, wayne).
		parent(ralph, martha).
		parent(wayne, amy).
		parent(neal, nate).
		parent(neal, ian).
		parent(neal, bethany).
		parent(john, debbie).
		parent(john, sean).
		parent(john, elizabeth).
		parent(sean, kyle).
		parent(sean, daniel).
		parent(lydia, ralph).
		parent(may, alberta).
		parent(alberta, neal).
		parent(alberta, wayne).
		parent(alberta, martha).
		parent(debbie, nate).
		parent(debbie, ian).
		parent(debbie, bethany).
		parent(cleo, debbie).
		parent(cleo, sean).
		parent(cleo, elizabeth).
		parent(kathy, kyle).
		parent(kathy, daniel).
		parent(joan, amy).

		male(albertus).
		male(albert).
		male(ralph).
		male(neal).
		male(wayne).
		male(nate).
		male(ian).
		male(john).
		male(sean).
		male(kyle).
		male(daniel).
		male(shane).
		female(lydia).
		female(may).
		female(alberta).
		female(joan).
		female(amy).
		female(bethany).
		female(debbie).
		female(cleo).
		female(elizabeth).
		female(kathy).
		female(martha).
		female( pam ).            

		child( C, P ) :- parent( P, C ).
		
		father( F, C ) :- male( F ), parent( F, C ).
		
		mother( M, C ) :- female( M ), parent( M, C ).
		
		son( Y, X ) :-  male(Y), child( Y, X ).
		
		daughter( Y, X ) :- female(Y), child( Y, X ).
		
		descendent(Y, X) :- ancestor( X, Y ).
		
		grandparent( X, Z ) :-
			parent( X, Y ),     
			parent( Y, Z ).
			
		sibling( X, Y ) :-
			father( Z, X ), father( Z, Y ),
			mother(W, X), mother( W, Y ),
			X \= Y.
			
		brother( X, Y ) :- male( X ), sibling( X, Y ).
		
		sister( X, Y ) :- female(X), sibling( X, Y ).
		
		ancestor( X, Z) :- parent( X, Z). 
		ancestor( X, Z)  :-
			parent( X, Y),
			ancestor( Y, Z).
				
		auntoruncle(X, W) :- sibling(X, Y), parent(Y, W).
		
		uncle(X, W) :- male(X), auntoruncle(X, W).
		
		aunt(X, W) :- female(X), auntoruncle(X, W).
		
		cousin( X, Y ) :- parent( Z, X ), auntoruncle( Z, Y ).
	""" )
	
		queryOnce( p, "aunt( U, _ ), parent( U, _ )." ) shouldBe "U = debbie"
	}
}