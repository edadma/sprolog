package ca.hyperreal.sprolog

import collection.mutable.{HashMap, ArrayBuffer, Buffer, ArrayStack}
import collection.immutable.SortedMap


class WAM
{
	var program: Program = _
	
	protected val trace = false
	protected val step = false
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
	protected val callables = new HashMap[Indicator, WAMInterface => Unit]
	protected val interface =
		new WAMInterface( this )
		{
			val bstack = wam.bstack
			
			val estack = wam.estack
			
			val x = wam.x
			
			def backtrack = wam.backtrack
			
			def unwind( size: Int ) = wam.unwind( size )
			
			def trail( a: Addr ) = wam.trail( a )
			
			def bind( a1: Addr, a2: Addr ) = wam.bind( a1, a2 )
			
			def unify( a1: Addr, a2: Addr ) = wam.unify( a1, a2 )
		}
	
	def addCallable( name: String, arity: Int, callable: WAMInterface => Unit )
	{
	val ind = Indicator( Symbol(name), arity )
	
		if (callables contains ind)
			sys.error( s"callable $ind already added" )
		else
			callables(ind) = callable
	}
	
	def query( qc: Query )
	{
		if (execute( qc ))
			println( "no" )
		else
		{
			if (bindings isEmpty)
				println( "yes" )
			else
			{
				while (success)
				{
					println( Prolog.display(bindings).map({case (k, v) => s"$k = $v"}).mkString(", ") )
					resume
				}
			}
		}
	}
	
	def queryFirst( qc: Query )
	{
		if (execute( qc ))
			println( "no" )
		else
		{
			if (bindings isEmpty)
				println( "yes" )
			else
				println( Prolog.display(bindings).map({case (k, v) => s"$k = $v"}).mkString(", ") )
		}
	}
	
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
		regs(1) = null
		run
	}
	
	def bindings = SortedMap[String, AST]( vars.toSeq.map( {case (k: Symbol, a: Addr) => k.name -> read( a )} ): _* )

	def success = !fail
	
	def resume =
		if (fail)
			false
		else
		{
			backtrack
			run
		}
	
	def unbound( a: Addr ) =
		a.read match
		{
			case PtrCell('ref, ptr ) if ptr == a => true
			case _ => false
		}
	
	def deref( store: Store, a: Int ): Addr = deref( new Addr(store, a) )
	
	def deref( a: Addr ): Addr =
		a.read match
		{
			case PtrCell( 'ref, v ) if v != a => deref( v )
			case _ => a
		}
	
	def read( a: Addr ): AST =
	{
		def str( p: Addr ) =
		{
		val FunCell( f, n ) = p.read
		
			StructureAST( f, for (i <- 1 to n) yield read( p + i ) )
//				f.name + "(" + (for (i <- 1 to n) yield p + i).mkString(",") + ")"
		}
		
		deref( a ).read match
		{
			case PtrCell( 'ref, a ) => VariableAST( Symbol(a.store.name + a.ind) )
			case PtrCell( 'str, p ) => str( p )
			case ConCell( c ) =>
				c match
				{
					case s: Symbol => AtomAST( s )
					case n: Number => NumberAST( n )
				}
		}
	}
	
	def addr( arg: Int ) = new Addr(x, arg)
	
	def read( arg: Int ): AST = read( addr(arg) )
	
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
	
	protected def variable( v: Symbol, b: Int, n: Int, a: Addr )
	{
		if (v ne null)
			vars += (v -> a)
	}
	
	protected def perform( inst: Instruction )
	{
		if (trace)
		{
			println( s"${p - 1}: $inst" )
			
			if (step)
				io.StdIn.readLine
		}
		
		inst match
		{
			case PutStructureInstruction( f, i ) =>
				put( h, f )
				put( x, i, str(h) )
				h += 1
// 				put( h, str(h + 1) )
// 				put( h + 1, f )
// 				put( x, i, h.read )
// 				h += 2
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
						
//						if (addr != h)
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
				
//				s += 1
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
				
//				s += 1
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
					case None =>
						callables.get( f ) match
						{
							case None => backtrack
							case Some( callable ) => callable( interface )
						}
					case Some( loc ) =>
						argc = f.arity
						cp = p	//p is incremented prior to instruction execution
						p = loc
				}
			case ProceedInstruction() =>
				p = cp
			case AllocateInstruction( n ) =>
				estack = new Frame( estack, cp, n )
				regs(1) = estack.perm
			case DeallocateInstruction =>
				p = estack.cp
				estack = estack.prev
				
				if (estack eq null)
					regs(1) = null
				else
					regs(1) = estack.perm
			case TryMeElseInstruction( t ) =>
				bstack = new Choice( bstack, x, argc, estack, cp, p + t.offset, tr.size, h )
				hb = h
			case RetryMeElseInstruction( t ) =>
				bstack.restore
				estack = bstack.estack
				regs(1) = estack.perm
				cp = bstack.cp
				bstack.bp = p + t.offset
				unwind( bstack.tr )
				h = bstack.h
				hb = h
			case TrustMeInstruction() =>
				bstack.restore
				estack = bstack.estack
				regs(1) = estack.perm
				cp = bstack.cp
				unwind( bstack.tr )
				h = bstack.h
				hb = h	// this may be wrong, pg. 57 wambook.pdf
				bstack = bstack.prev
			case PutConstantInstruction( c, i ) =>
				put( x, i, ConCell(c) )
			case GetConstantInstruction( c, i ) =>
				val addr = deref( x, i )
				
				addr.read match
				{
					case PtrCell( 'ref, _ ) =>
						addr write ConCell( c )
						trail( addr )
					case ConCell( const ) =>
						if (const != c)
							backtrack
					case _ => backtrack
				}
			case SetConstantInstruction( c ) =>
				put( h, ConCell(c) )
				h += 1
			case UnifyConstantInstruction( c ) =>
				mode match
				{
					case ReadMode =>
						val addr = deref( s )
						
						addr.read match
						{
							case PtrCell( 'ref, _ ) =>
								addr write ConCell( c )
								trail( addr )
							case ConCell( const ) =>
								if (const != c)
									backtrack
							case _ => backtrack
						}
						
						s += 1
					case WriteMode =>
						put( h, ConCell(c) )
						h += 1
				}
				
			case PutListInstruction( i ) =>
				put( x, i, LisCell(h) )
			case GetListInstruction( i ) =>
				val addr = deref( x, i )
				
				addr.read match
				{
					case PtrCell( 'ref, _ ) =>
						put( h, LisCell(h + 1) )
						bind( addr, h )
						h += 1
						mode = WriteMode
					case LisCell( a ) =>
						s = a
						mode = ReadMode
					case _ => backtrack
				}
			case SetVoidInstruction( n ) =>
				for (i <- 0 until n)
				{
					put( h, ref(h) )
					h += 1
				}
			case UnifyVoidInstruction( n ) =>
				mode match
				{
					case ReadMode => s += n
					case WriteMode =>
						for (i <- 0 until n)
						{
							put( h, ref(h) )
							h += 1
						}
				}
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
		{
			sys.error( "neither address is unbound" )
		}
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
				(d1.read, d2.read) match
				{
					case (PtrCell( 'ref, _ ), _)|(_, PtrCell( 'ref, _ )) => bind( d1, d2 )
					case (ConCell( c1 ), ConCell( c2 )) => failure = c1 != c2
					case (PtrCell( 'str, v1 ), PtrCell( 'str, v2 )) =>
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
		
		failure
	}
}

abstract class Instruction
{
	private var l: Any = _
	
	def label = l
	
	def target( l: Any ) =
	{
		this.l = l
		this
	}
}

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
case class CallInstruction( f: Indicator ) extends Instruction
case class ProceedInstruction() extends Instruction
case class AllocateInstruction( n: Int ) extends Instruction
case object DeallocateInstruction extends Instruction
case class PutConstantInstruction( c: Any, i: Int ) extends Instruction
case class GetConstantInstruction( c: Any, i: Int ) extends Instruction
case class SetConstantInstruction( c: Any ) extends Instruction
case class UnifyConstantInstruction( c: Any ) extends Instruction
case class PutListInstruction( i: Int ) extends Instruction
case class GetListInstruction( i: Int ) extends Instruction
case class SetVoidInstruction( n: Int ) extends Instruction
case class UnifyVoidInstruction( n: Int ) extends Instruction
case class TryMeElseInstruction( t: Label ) extends Instruction
case class RetryMeElseInstruction( t: Label ) extends Instruction
case class TrustMeInstruction() extends Instruction

trait Cell
case class PtrCell( typ: Symbol, k: Addr ) extends Cell
case class FunCell( f: Symbol, n: Int ) extends Cell
case class ConCell( c: Any ) extends Cell
case class LisCell( a: Addr ) extends Cell

trait Mode
case object ReadMode extends Mode
case object WriteMode extends Mode

class Label( src: Int )
{
	private var d: Int = _
	private var set = false
	
	val n = Label.next
	
	def backpatch( targ: Int )
	{
		d = targ - src - 1
		set = true
	}
	
	def offset =
		if (!set)
			sys.error( toString )
		else
			d
	
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
	val argregs = regs.take( n + 1 ).toVector
	
	def restore
	{
		for (i <- 0 until n + 1)
			regs(i) = argregs(i)
	}
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

class Program( val code: IndexedSeq[Instruction], val procmap: collection.Map[Indicator, Int] )
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

abstract class WAMInterface( val wam: WAM )
{
	val estack: Frame
	val bstack: Choice
	val x: Store
	
	def backtrack
	
	def unwind( size: Int )
	
	def trail( a: Addr )
	
	def bind( a1: Addr, a2: Addr )
	
	def unify( a1: Addr, a2: Addr ): Boolean
}