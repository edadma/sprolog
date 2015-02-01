package ca.hyperreal.swam

import collection.mutable.{HashMap, ArrayBuffer, Buffer, ArrayStack}
import collection.immutable.SortedMap


class WAM
{
	var program: Program = _
	
	protected val trace = false
	protected val QUERY = 1000000000	
	protected val heap = new Store( "H", 10000 )
	protected val x = new Store( "X", 100 )
	protected val pdl = new ArrayStack[Addr]
	protected val tr = new ArrayStack[Addr]
	protected var estack: Frame = null
	protected var bstack: Choice = null
	protected var h: Addr = _
	protected var hb: Addr = _
	protected var s: Addr = _
	protected var fail: Boolean = _
	protected var mode: Mode = _
	protected var query: Query = _
	protected var p: Int = _
	protected var cp : Int = _
	protected val vars = new ArrayBuffer[(Symbol, Addr)]
	protected val regs = Array[Store]( x, null )	// second element points to current environment variable store
	protected var argc: Int = _
	
	def execute( q: Query ) =
	{
		fail = false
		h = new Addr( heap, 0 )
		tr.clear
		estack = null
		bstack = null
		query = q
		p = QUERY
		cp = -1
		vars.clear
		run
	}
	
	def bindings = SortedMap( vars.toSeq.map( {case (k: Symbol, a: Addr) => k.name -> read( a )} ): _* )

	def alternative = !fail
	
	def continue =
		if (fail)
			false
		else
		{
			backtrack
			run
		}
	
	protected def run =
	{
		while (p > -1 && !fail)
		{
		val _p = p
		
			p += 1
			
			perform( if (p < QUERY) program.code(_p) else query.code(_p - QUERY) )
		}
		
		fail
	}
		
	protected def put( a: Addr, c: Cell )
	{
		put( a.store, a.ind, c )
	}
	
	protected def put( r: Store, index: Int, c: Cell )
	{
		if (r.size <= index)
			r ++= Seq.fill[Cell]( index - r.size + 1 )( null )
			
		r(index) = c
	}
	
	protected def deref( store: Store, a: Int ): Addr = deref( new Addr(store, a) )
	
	protected def deref( a: Addr ): Addr =
		a.read match
		{
			case PtrCell( 'ref, v ) if v != a => deref( v )
			case _ => a
		}
	
	protected def read( a: Addr ): String =
		deref( a ).read match
		{
			case PtrCell( 'ref, a ) => a.store.name + a.ind
			case PtrCell( 'str, p ) =>
				val FunCell( f, n ) = p.read
				
				if (n == 0)
					f.name
				else
 					f.name + "(" + (for (i <- 1 to n) yield read( p + i )).mkString(",") + ")"
//					f.name + "(" + (for (i <- 1 to n) yield p + i).mkString(",") + ")"
		}
	
	protected def variable( v: Symbol, b: Int, n: Int, a: Addr )
	{
		if (v ne null)
			vars += (v -> a)
	}
	
	protected def perform( inst: Instruction )
	{
		if (trace)
		{
			println( inst )
			io.StdIn.readLine
		}
		
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
						mode = WriteMode
					case PtrCell( 'str, a ) =>
						if (a.read == f)
						{
							s = a + 1
							mode = ReadMode
						}
						else
							backtrack
					case _ =>
						backtrack
				}
			case UnifyVariableInstruction( b, i ) =>
				mode match
				{
					case ReadMode =>
						put( regs(b), i, s.read )
						s += 1
					case WriteMode =>
						put( h, ref(h) )
						put( regs(b), i, h.read )
						h += 1
				}
/*				
				s += 1*/
			case UnifyValueInstruction( b, i ) =>
				mode match
				{
					case ReadMode =>
						if (unify( new Addr(regs(b), i), s ))
							backtrack
							
						s += 1
					case WriteMode =>
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
				if (unify( new Addr(regs(b), n), new Addr(x, i) ))
					backtrack
			case CallInstruction( f ) =>
				program.procmap.get( f ) match
				{
					case Some( loc ) =>
						argc = f.n
						cp = p	//p is incremented prior to instruction execution
						p = loc
					case None => backtrack
				}
			case ProceedInstruction =>
				p = cp
			case AllocateInstruction( n ) =>
				estack = new Frame( estack, cp, n )
				regs(1) = estack.perm
			case DeallocateInstruction =>
				regs(1) = estack.perm
				p = estack.cp
				estack = estack.prev
			case TryMeElseInstruction( l ) =>
				bstack = new Choice( bstack, x, argc, estack, cp, l.ref, tr.size, h )
				hb = h
			case RetryMeElseInstruction( l ) =>
				bstack.argregs copyToBuffer x
				estack = bstack.estack
				cp = bstack.cp
				bstack.bp = l.ref
				unwind( bstack.tr )
				h = bstack.h
				hb = h
			case TrustMeInstruction =>
				bstack.argregs copyToBuffer x
				estack = bstack.estack
				cp = bstack.cp
				unwind( bstack.tr )
				h = bstack.h
				hb = h
				bstack = bstack.prev
		}
		
		if (trace)
		{
			println( s"mode: $mode  H: $h  S: $s" )
			println( x )
			
			if (estack ne null)
				println( estack.perm )
				
			println( heap )
			println
		}
	}
	
	protected def ref( a: Addr ) = PtrCell( 'ref, a )
	
	protected def str( a: Addr ) = PtrCell( 'str, a )
	
	protected def backtrack
	{
		if (bstack eq null)
		{
			p = -1
			fail = true
		}
		else
			p = bstack.bp
	}
	
	protected def unwind( size: Int )
	{
		while (tr.size > size)
		{
		val a = tr.pop
		
			a write ref( a )
		}
	}
	
	protected def trail( a: Addr )
	{
		tr push a
	}
	
	protected def unbound( a: Addr ) =
		a.read match
		{
			case PtrCell('ref, ptr ) if ptr == a => true
			case _ => false
		}

	protected def bind( a1: Addr, a2: Addr )
	{
		if (unbound( a1 ))
		{
			put( a1, a2.read )
			trail( a1 )
		}
		else if (unbound( a2 ))
		{
			put( a2, a1.read )
			trail( a2 )
		}
		else
			sys.error( "neither address is unbound" )
	}
	
	protected def unify( a1: Addr, a2: Addr ) =
	{
	var failure = false
	
		pdl.clear
		pdl push a1
		pdl push a2
		
		while (!(pdl.isEmpty || failure))
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
						failure = true
				}
			}
		}
		
		failure
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
case class TryMeElseInstruction( l: Label ) extends Instruction
case class RetryMeElseInstruction( l: Label ) extends Instruction
case object TrustMeInstruction extends Instruction

trait Cell
case class PtrCell( typ: Symbol, k: Addr ) extends Cell
case class FunCell( f: Symbol, n: Int ) extends Cell

trait Mode
case object ReadMode extends Mode
case object WriteMode extends Mode

class Label
{
	private var p: Int = _
	private var set = false
	
	val n = Label.next
	
	def backpatch( p: Int )
	{
		this.p = p
		set = true
	}
	
	def ref =
		if (!set)
			sys.error( toString )
		else
			p
	
	override def toString = "L" + n + (if (!set) " UNPATCHED" else "")
}

object Label
{
	private var n = 0
	
	def next =
	{
		n += 1
		n
	}
}

class Frame( val prev: Frame, val cp: Int, n: Int )
{
	val perm = new Store( "Y", n )
}

class Choice( val prev: Choice, regs: Store, n: Int, val estack: Frame, val cp: Int, var bp: Int, val tr: Int, val h: Addr )
{
	val argregs = regs.take( n ).toVector
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

class Addr( val store: Store, val ind: Int ) extends Ordered[Addr]
{
	def read = store(ind)

	def write( c: Cell ) = store(ind) = c
	
	def compare( that: Addr ) =
	{
		if (store ne that.store)
			sys.error( s"$this and $that are not in the same 'store'" )
			
		this.ind - that.ind
	}
	
	def read( from: Seq[Cell] ): Cell =
		if (from ne store)
			sys.error( "incorrect store" )
		else
			read
	
	def +( inc: Int ) = if (inc == 0) this else new Addr( store, ind + inc )
	
	override def equals( that: Any ) = that.isInstanceOf[Addr] && (this.store eq that.asInstanceOf[Addr].store) && this.ind == that.asInstanceOf[Addr].ind
	
	override def toString = s"[${store.name} $ind]"
}