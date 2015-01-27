package ca.hyperreal.swam

import collection.mutable.{HashMap, ArrayBuffer, Buffer, ArrayStack}


class WAM
{
	val trace = false
	
	val heap = new Store( "H" )
	val x = new Store( "X" )
	val pdl = new ArrayStack[Addr]
	var h = new Addr( heap, 0 )
	var s: Addr = h
	var fail = false
	var mode = 'read
	var code: Code = _
	var p: Int = -1
	
	def put( a: Addr, c: Cell )
	{
		put( a.store, a.ind, c )
	}
	
	def put( r: Store, index: Int, c: Cell )
	{
		if (r.size <= index)
			r ++= Seq.fill[Cell]( index - r.size + 1 )( null )
			
		r(index) = c
	}
	
	def deref( store: Store, a: Int ): Addr = deref( new Addr(store, a) )
	
	def deref( a: Addr ): Addr =
		a.read match
		{
			case PtrCell( 'ref, v ) if v != a => deref( v )
			case _ => a
		}

	def bindings( varmap: collection.Map[Symbol, Int] ) = varmap map {case (k, v) => k.name -> read( new Addr(x, v) )}
	
	def read( a: Addr ): String =
		deref( a ).read match
		{
			case PtrCell( 'ref, a ) => a.store.name + a.ind
			case PtrCell( 'str, p ) =>
				val FunCell( f, n ) = p.read
				
				if (n == 0)
					f.name
				else
					f.name + "(" + (for (i <- 1 to n) yield read( p + i )).mkString(",") + ")"
		}
	
	def execute( seq: Seq[Instruction] ): Boolean =
	{
		fail = false

		for (inst <- seq)
			if (execute( inst ))
				return true
		
		while (p > -1)
		{
			if (execute( code.program(p) ))
				return true
				
			p += 1
		}
		
		false
	}
	
	def execute( inst: Instruction ) =
	{
		inst match
		{
			case PutStructureInstruction( f, i ) =>
				put( h, str(h + 1) )
				put( h + 1, f )
				put( x, i, h.read )
				h += 2
			case SetVariableInstruction( i ) =>
				put( h, ref(h) )
				put( x, i, h.read )
				h += 1
			case SetValueInstruction( i ) =>
				put( h, x(i) )
				h += 1
			case GetStructureInstruction( f, i ) =>
				val addr = deref( x, i )
				
				addr.read match
				{
					case PtrCell( 'ref, _ ) =>
						put( h, str(h + 1) )
						put( h + 1, f )
						bind( addr, h )
						h += 2
						mode = 'write
					case PtrCell( 'str, a ) =>
						if (a.read == f)
						{
							s = a + 1
							mode = 'read
						}
						else
							fail = true
					case _ => fail = true
				}
			case UnifyVariableInstruction( i ) =>
				mode match
				{
					case 'read => put( x, i, s.read )
					case 'write =>
						put( h, ref(h) )
						put( x, i, h.read )
						h += 1
				}
				
				s += 1
			case UnifyValueInstruction( i ) =>
				mode match
				{
					case 'read => unify( new Addr(x, i), s )
					case 'write =>
						put( h, x(i) )
						h += 1
				}
				
				s += 1
			case PutVariableInstruction( n, i ) =>
				put( h, ref(h) )
				put( x, n, h.read )
				put( x, i, h.read )
				h += 1
			case PutValueInstruction( n, i ) =>
				put( x, i, x(n) )
			case GetVariableInstruction( n, i ) =>
				put( x, n, x(i) )
			case GetValueInstruction( n, i ) =>
				unify( new Addr(x, n), new Addr(x, i) )
			case CallInstruction( f ) =>
				code.procmap.get( f ) match
				{
					case Some( loc ) => p = loc
					case None => fail = true
				}
			case ProceedInstruction =>
				p = -2
		}
		
		if (trace)
		{
			println( inst )
			println( x )
			println( heap )
			println
		}
		
		fail
	}
	
	def ref( a: Addr ) = PtrCell( 'ref, a )
	
	def str( a: Addr ) = PtrCell( 'str, a )
	
	def unbound( a: Addr ) =
		a.read match
		{
			case PtrCell('ref, p ) if p == a => true
			case _ => false
		}

	def bind( a1: Addr, a2: Addr )
	{
		if (unbound( a1 ))
			put( a1, ref(a2) )
		else if (unbound( a2 ))
			put( a2, ref(a1) )
		else
			sys.error( "neither address is unbound" )
	}
	
	def unify( a1: Addr, a2: Addr )
	{
		pdl push a1
		pdl push a2
		fail = false
		
		while (!(pdl.isEmpty || fail))
		{
		val d1 = deref( pdl pop )
		val d2 = deref( pdl pop )
		
			if (d1 != d2)
			{
			val PtrCell( t1, v1 ) = d1.read
			val PtrCell( t2, v2 ) = d2.read
			
				if (t1 == 'ref || t2 == 'ref)
					bind( d1, d2 )
				else
				{
				val f1@FunCell( _, n ) = v1.read
				val f2 = v2.read
				
					if (f1 == f2)
						for (i <- 1 to n)
						{
							pdl push (v1 + i)
							pdl push (v2 + i)
						}
					else
						fail = true
				}
			}
		}
	}
}

trait Instruction

case class PutStructureInstruction( f: FunCell, i: Int ) extends Instruction
case class SetVariableInstruction( i: Int ) extends Instruction
case class SetValueInstruction( i: Int ) extends Instruction
case class GetStructureInstruction( f: FunCell, i: Int ) extends Instruction
case class UnifyVariableInstruction( i: Int ) extends Instruction
case class UnifyValueInstruction( i: Int ) extends Instruction
case class PutVariableInstruction( n: Int, i: Int ) extends Instruction
case class PutValueInstruction( n: Int, i: Int ) extends Instruction
case class GetVariableInstruction( n: Int, i: Int ) extends Instruction
case class GetValueInstruction( n: Int, i: Int ) extends Instruction
case class CallInstruction( f: FunCell ) extends Instruction
case object ProceedInstruction extends Instruction

trait Cell

case class PtrCell( typ: Symbol, k: Addr ) extends Cell
case class FunCell( f: Symbol, n: Int ) extends Cell

class Store( val name: String ) extends ArrayBuffer[Cell]

case class Code( program: IndexedSeq[Instruction], procmap: collection.Map[FunCell, Int] )

class Addr( val store: Store, val ind: Int )
{
	def read = store(ind)
	
	def read( from: Seq[Cell] ): Cell =
		if (from ne store)
			sys.error( "incorrect store" )
		else
			read
		
	def +( inc: Int ) = if (inc == 0) this else new Addr( store, ind + inc )
	
	override def equals( that: Any ) = that.isInstanceOf[Addr] && (this.store eq that.asInstanceOf[Addr].store) && this.ind == that.asInstanceOf[Addr].ind
	
	override def toString = s"[${store.name} $ind]"
}