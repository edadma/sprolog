package ca.hyperreal.swam

import collection.mutable.{HashMap, ArrayBuffer, Buffer, ArrayStack}
import collection.immutable.SortedMap


class WAM
{
	val trace = false
	
	val QUERY = 1000000000
	
	val heap = new Store( "H", 10000 )
	val x = new Store( "X", 100 )
	val pdl = new ArrayStack[Addr]
	val stack = new ArrayStack[Frame]
	var h: Addr = _
	var s: Addr = _
	var fail = false
	var mode = 'read
	var query: Query = _
	var program: Program = _
	var p: Int = _
	var cp : Int = _
	val varmap = new HashMap[(Int, Int), Symbol]
	val vars = new ArrayBuffer[(Symbol, Addr)]
	val regs = new Array[Store]( 2 )
	
	regs(0) = x
	
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

// 	def bindingmap( vars: Seq[(String, Int)] ) = vars.map( {case (k: String, v: Int) => k -> x(v).asInstanceOf[PtrCell].k} )
// 	
// 	def bindings( vars: Seq[(String, Int)] ) = vars.map( {case (k: String, v: Int) => k -> read( new Addr(x, v) )} )
// 	
// 	def bindings( varmap: collection.Map[Symbol, Int] ): collection.Map[String, String] =
// 		SortedMap( varmap.toSeq.map( {case (k: Symbol, v: Int) => k.name -> read( new Addr(x, v) )} ): _* )
	
	def bindings = SortedMap( vars.toSeq.map( {case (k: Symbol, a: Addr) => k.name -> read( a )} ): _* )
	
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
	
	def execute( q: Query ): Boolean =
	{
		fail = false
		h = new Addr( heap, 0 )
		query = q
		p = QUERY
		cp = -1
		
		while (p > -1)
		{
		val _p = p
		
			p += 1
			
			if (execute( if (p < QUERY) program.code(_p) else query.code(_p - QUERY) ))
				return true
		}
		
		false
	}
	
	private def variable( v: Symbol, b: Int, n: Int, a: Addr )
	{
		if (v ne null)
			vars += (v -> a)
	}
	
	def execute( inst: Instruction ) =
	{
		if (trace)
			println( inst )
			
		inst match
		{
			case PutStructureInstruction( f, i ) =>
				put( h, str(h + 1) )
				put( h + 1, f )
				put( x, i, h.read )
				h += 2
			case SetVariableInstruction( v, b, i ) =>
				variable( v, b, i, h )
				put( h, ref(h) )
				put( regs(b), i, h.read )
				h += 1
			case SetValueInstruction( b, i ) =>
				put( h, regs(b)(i) )
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
					case _ =>
						fail = true
				}
			case UnifyVariableInstruction( b, i ) =>
				mode match
				{
					case 'read =>
						put( regs(b), i, s.read )
						s += 1
					case 'write =>
						put( h, ref(h) )
						put( regs(b), i, h.read )
						h += 1
				}
/*				
				s += 1*/
			case UnifyValueInstruction( b, i ) =>
				mode match
				{
					case 'read =>
						unify( new Addr(regs(b), i), s )
						s += 1
					case 'write =>
						put( h, regs(b)(i) )
						h += 1
				}
/*				
				s += 1*/
			case PutVariableInstruction( v, b, n, i ) =>
				variable( v, b, n, h )
				put( h, ref(h) )
				put( regs(b), n, h.read )
				put( x, i, h.read )
				h += 1
			case PutValueInstruction( b, n, i ) =>
				put( x, i, regs(b)(n) )
			case GetVariableInstruction( b, n, i ) =>
				put( regs(b), n, x(i) )
			case GetValueInstruction( b, n, i ) =>
				unify( new Addr(regs(b), n), new Addr(x, i) )
			case CallInstruction( f ) =>
				program.procmap.get( f ) match
				{
					case Some( loc ) =>
						cp = p	//p is incremented prior to instruction execution
						p = loc
					case None => fail = true
				}
			case ProceedInstruction =>
				p = cp
			case AllocateInstruction( n ) =>
				val fr = new Frame( cp, n )
				stack.push( fr )
				regs(1) = fr.perm
			case DeallocateInstruction =>
				regs(1) = stack.top.perm
				p = stack.pop.cp
		}
		
		if (trace)
		{
			println( s"mode: $mode  H: $h  S: $s" )
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
			case PtrCell('ref, ptr ) if ptr == a => true
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
case class SetVariableInstruction( v: Symbol, b: Int, i: Int ) extends Instruction
case class SetValueInstruction( b: Int, i: Int ) extends Instruction
case class GetStructureInstruction( f: FunCell, i: Int ) extends Instruction
case class UnifyVariableInstruction( b: Int, i: Int ) extends Instruction
case class UnifyValueInstruction( b: Int, i: Int ) extends Instruction
case class PutVariableInstruction( v: Symbol, b: Int, n: Int, i: Int ) extends Instruction
case class PutValueInstruction( b: Int, n: Int, i: Int ) extends Instruction
case class GetVariableInstruction( b: Int, n: Int, i: Int ) extends Instruction
case class GetValueInstruction( b: Int, n: Int, i: Int ) extends Instruction
case class CallInstruction( f: FunCell ) extends Instruction
case object ProceedInstruction extends Instruction
case class AllocateInstruction( n: Int ) extends Instruction
case object DeallocateInstruction extends Instruction

trait Cell

case class PtrCell( typ: Symbol, k: Addr ) extends Cell
case class FunCell( f: Symbol, n: Int ) extends Cell

class Frame( val cp: Int, n: Int )
{
	val perm = new Store( "Y", n )
}

class Store( val name: String, init: Int ) extends ArrayBuffer[Cell]( init )
{
	override def toString =
	{
	val buf = new StringBuilder
	
		buf append s"store: $name\n"
		
		for (ind <- 0 until length)
			buf append f"  $ind%2d " + this(ind) + "\n"
			
		buf.toString
	}
}

class Program( val code: IndexedSeq[Instruction], val procmap: collection.Map[FunCell, Int] )
{
	override def toString = code + "\n" + procmap
}

class Query( val code: IndexedSeq[Instruction] )
{
	override def toString = code.toString
}

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