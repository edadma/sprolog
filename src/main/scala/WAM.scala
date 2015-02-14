package ca.hyperreal.sprolog

import collection.mutable.{HashMap, ArrayBuffer, Buffer, ArrayStack}
import collection.immutable.SortedMap


class WAM
{
	var db: Database = _
	var ops: OperatorTable = _
	
	protected val trace = false
	protected val step = false
	protected val QUERY = 1000000000	
	protected val heap = new Store( "H", 10000 )
	protected val x = new Store( "X", 100 )
	protected val pdl = new ArrayStack[Address]
	protected val trail = new ArrayStack[Addr]
	protected var estack: Frame = null
	protected var bstack: Choice = null
	protected var b0: Choice = null
	protected var h: Addr = _
	protected var hb: Addr = _
	protected var s: Addr = _
	protected var fail: Boolean = _
	protected var mode: Mode = _
	protected var callcode: ArrayBuffer[Instruction] = _
	protected var p: Int = _
	protected var cp : Int = _
	protected val vars = new ArrayBuffer[(Symbol, Addr)]
	protected val regs = Array[Store]( x, null )	// second element points to current environment variable store
	protected var argc: Int = _
	protected val callables = new HashMap[Indicator, WAMInterface => Boolean]
	protected val interface =
		new WAMInterface( this )
		{
			val bstack = wam.bstack
			
			val estack = wam.estack
			
			val x = wam.x
			
			def backtrack = wam.backtrack
			
			def unwind( size: Int ) = wam.unwind( size )
			
			def trail( a: Addr ) = wam.trail( a )
			
			def bind( a1: Address, a2: Address ) = wam.bind( a1, a2 )
			
			def unify( a1: Address, a2: Address ) = wam.unify( a1, a2 )
		}
	
	def define( name: String, arity: Int )( c: => Boolean )
	{
		define( name, arity, _ => c )
	}
	
	def define( name: String, arity: Int, c: WAMInterface => Boolean )
	{
	val ind = Indicator( Symbol(name), arity )
	
		if (callables contains ind)
			sys.error( s"callable $ind already added" )
		else
			callables(ind) = c
	}
	
	def query( qc: ArrayBuffer[Instruction] )
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
					println( display(bindings).map({case (k, v) => s"$k = $v"}).mkString(", ") )
					resume
				}
			}
		}
	}
	
	def queryOnce( qc: ArrayBuffer[Instruction] )
	{
		if (execute( qc ))
			println( "no" )
		else
		{
			if (bindings isEmpty)
				println( "yes" )
			else
				println( display(bindings).map({case (k, v) => s"$k = $v"}).mkString(", ") )
		}
	}
	
	def execute( q: ArrayBuffer[Instruction] ) =
	{
		fail = false
		h = new Addr( heap, 0 )
		trail.clear
		estack = null
		bstack = null
		callcode = q
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
	
	def unbound( a: Address ) =
		a.read match
		{
			case RefCell( ptr ) if ptr == a => true
			case _ => false
		}
	
	def deref( store: Store, a: Int ): Address = deref( new Addr(store, a) )
	
	def deref( a: Address ): Address =
		a.read match
		{
			case RefCell( v ) if v != a => deref( v )
			case _ => a
		}
	
	def read( a: Address ): AST =
	{
		deref( a ).read match
		{
			case RefCell( a: Addr ) => a
			case StrCell( p: Addr ) =>
				val FunCell( f, n ) = p.read
				
				StructureAST( f, for (i <- 1 to n) yield read( p + i ) )
			case LisCell( a ) =>
				StructureAST( DOT, IndexedSeq(read(a), read(a + 1)) )
			case ConCell( c ) =>
				c match
				{
					case s: Symbol => AtomAST( s )
					case n: Number => NumberAST( n )
					case s: String => StringAST( s )
					case _ => ConstantAST( c )
				}
		}
	}
	
	def write( term: AST ): Cell =
	{
		def writeseq( cells: Seq[Cell] ) =
		{
		val start = h
		
			for (c <- cells)
			{
				put( h, c )
				h += 1
			}
			
			start
		}
		
		term match
		{
			case AtomAST( atom, _ ) => ConCell( atom )
			case NumberAST( n, _ ) => ConCell( n )
			case StringAST( s, _ ) => ConCell( s )
			case ConstantAST( c, _ ) => ConCell( c )
			case StructureAST( DOT, Seq(l, r), _ ) => LisCell( writeseq(Seq(write(l), write(r))) )
			case StructureAST( f, args, _ ) => StrCell( writeseq(FunCell(f, args.length) +: (for (a <- args) yield write(a))) )
		}
	}

// 	def write( term: AST, a: Addr ): Addr =
// 	{
// 		term match
// 		{
// 			case AtomAST( atom, _ ) =>
// 				put( a, ConCell(atom) )
// 				a + 1
// 			case NumberAST( n, _ ) =>
// 				put( a, ConCell(n) )
// 				a + 1
// 			case StringAST( s, _ ) =>
// 				put( a, ConCell(s) )
// 				a + 1
// 			case ConstantAST( c, _ ) =>
// 				put( a, ConCell(c) )
// 				a + 1
// 			case StructureAST( DOT, Seq(l, r), _ ) =>
// 				put( a, LisCell(a + 1) )
// 				write( r, write(l, a + 2) )
// 			case StructureAST( f, args, _ ) =>
// 				def writearg( ind: Int, a: Addr  ): Addr =
// 				{
// 					if (ind == args.length)
// 						a
// 					else
// 						writearg( ind + 1, write(args(ind), a) )
// 				}
// 				
// 				put( a, PtrCell('str, a + 1) )
// 				put( a + 1, FunCell(f, args.length) )
// 				writearg( 0, a + 2 )
// 		}
// 	}
	
	def display( m: Map[String, AST] ): Map[String, String] = m map {case (k, v) => k -> display( v )}
	
	def display( a: AST ): String =
	{
		def _display( f: Symbol, args: Seq[AST] ) =
			f.name + (for (a <- args) yield display( a )).mkString( "(", ", ", ")" )
			
		def _displayPrec( prec: Int, a: AST ): String =
			a match
			{
				case NumberAST( n, _ ) => n.toString
				case AtomAST( atom, _ ) => atom.name
				case StringAST( s, _ ) => s
				case VariableAST( s, _ ) => s.name
				case _: Addr => a.toString
				case s: StructureAST if isList( s ) => toList( s ).map( display(_) ).mkString( "[", ", ", "]" )
				case s: StructureAST if isVarList( s ) =>
					val list = toVarList( s )
					
					list.dropRight( 1 ).map( display(_) ).mkString( "[", ", ", "|" ) + display( list.last ) + "]"
				case StructureAST( f, args, _ ) =>
					if (args.length == 1)
					{
						ops.operator( f, 'prefix ) match
						{
							case None =>
								ops.operator( f, 'postfix ) match
								{
									case None => _display( f, args )
									case Some( inner_prec ) =>
										val sep = if (f.name.head.isLetter) " " else ""
										
										((if (inner_prec > prec) "(" else "") + _displayPrec( inner_prec, args(0) ) + sep + f.name +
											(if (inner_prec > prec) ")" else ""))
								}
							case Some( inner_prec ) =>
								val sep = if (f.name.head.isLetter) " " else ""
								
								((if (inner_prec > prec) "(" else "") + f.name + sep + _displayPrec( inner_prec, args(0) ) +
									(if (inner_prec > prec) ")" else ""))
						}
					}
					else if (args.length == 2)
					{
						ops.operator( f, 'infix ) match
						{
							case None => _display( f, args )
							case Some( inner_prec ) =>
								val sep = if (f == '+ || f == '- || f.name.head.isLetter) " " else ""
								
								((if (inner_prec > prec) "(" else "") + _displayPrec( inner_prec, args(0) ) + sep +
									f.name + sep + _displayPrec( inner_prec, args(1) ) +
									(if (inner_prec > prec) ")" else ""))
						}
					}
					else
						_display( f, args )
					
				case ConstantAST( c, _ ) => c.toString
			}
			
		_displayPrec( 10000, a )
	}
	
	def addr( a: Int ) = deref( new Addr(x, a) )
	
	def arg( a: Int ) = read( addr(a) )
	
	def argInstantiated( a: Int ) =
	{
	val v = arg( a )
	
		require( !v.isInstanceOf[Addr], "instantiation error" )
		v
	}
	
	def argNumber( a: Int ) =
	{
	val v = argInstantiated( a )
	
		require( v.isInstanceOf[NumberAST], "expected number" )
		v.asInstanceOf[NumberAST].n
	}
	
	def argInteger( a: Int ) =
	{
	val v = argNumber( a )
	
		require( v.isInstanceOf[Int], "expected integer" )
		v.asInstanceOf[Int]
	}

	def isInteger( t: AST ) = t.isInstanceOf[NumberAST] && t.asInstanceOf[NumberAST].n.isInstanceOf[Int]

	def asInteger( t: AST ) = t.asInstanceOf[NumberAST].n.asInstanceOf[Int]

	def asSymbol( t: AST ) = t.asInstanceOf[AtomAST].atom
	
	def structureArg( a: Address, n: Int ) = deref( a ).read match {case StrCell( s: Addr ) => s + n}
	
	def isCompound( a: Address ) =
		deref( a ).read match
		{
			case StrCell( _ ) => true
			case _ => false
		}
		
	protected def run =
	{
		while (p > -1 && !fail)
		{
		val _p = p
		
			p += 1
			
			perform( if (p < QUERY) db.instruction(_p) else callcode(_p - QUERY) )
		}
		
		fail
	}
	
	protected def put( a: Address, c: Cell )
	{
		put( a.asInstanceOf[Addr].store, a.asInstanceOf[Addr].ind, c )
	}
	
	protected def put( r: Store, index: Int, c: Cell )
	{
		if (r.size <= index)
			r ++= Seq.fill[Cell]( index - r.size + 1 )( null )
			
		r(index) = c
	}
	
	protected def binding( v: Symbol, b: Int, n: Int, a: Addr )
	{
		if (v ne null)
			vars += (v -> a)
	}
	
	protected def perform( inst: Instruction )
	{
		def trim
		{
			h = bstack.h
			heap.remove( h.ind, heap.size - h.ind )
			callcode.remove( bstack.callcode, callcode.size - bstack.callcode )
		}
		
		if (trace)
		{
			println( s"${p - 1}: $inst" )
			
			if (step)
				io.StdIn.readLine
		}
		
		inst match
		{
			case PutStructureInstruction( f, i ) =>
				// optimization for section 5.1 of wambook
				put( h, f )
				put( x, i, StrCell(h) )
				h += 1
// 				put( h, StrCell(h + 1) )
// 				put( h + 1, f )
// 				put( x, i, h.read )
// 				h += 2
			case SetVariableInstruction( v, b, i ) =>
				binding( v, b, i, h )
				put( h, RefCell(h) )
				put( regs(b), i, h.read )
				h += 1
			case SetValueInstruction( b, i ) =>
				put( h, regs(b)(i) )
				h += 1
			case GetStructureInstruction( f, i ) =>
				val addr = deref( x, i )
				
				addr.read match
				{
					case RefCell( _ ) =>
						put( h, StrCell(h + 1) )
						put( h + 1, f )
						
//						if (addr != h)
							bind( addr, h )
							
						h += 2
						mode = WriteMode
					case StrCell( a: Addr ) =>
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
						put( h, RefCell(h) )
						put( regs(b), i, h.read )
						h += 1
				}
				
//				s += 1
			case UnifyValueInstruction( b, i ) =>
				mode match
				{
					case ReadMode =>
						if (!unify( new Addr(regs(b), i), s ))
							backtrack
							
						s += 1
					case WriteMode =>
						put( h, regs(b)(i) )
						h += 1
				}
				
//				s += 1
			case PutVariableInstruction( v, b, n, i ) =>
				binding( v, b, n, h )
				put( h, RefCell(h) )
				put( regs(b), n, h.read )
				put( x, i, h.read )
				h += 1
			case PutValueInstruction( b, n, i ) =>
				put( x, i, regs(b)(n) )
			case GetVariableInstruction( b, n, i ) =>
				put( regs(b), n, x(i) )
			case GetValueInstruction( b, n, i ) =>
				if (!unify( new Addr(regs(b), n), new Addr(x, i) ))
					backtrack
			case CallInstruction( f ) =>
				db.address( f ) match
				{
					case None =>
						callables.get( f ) match
						{
							case None => backtrack
							case Some( callable ) =>
								if (!callable( interface ))
									backtrack
						}
					case Some( loc ) =>
						cp = p	//p is incremented prior to instruction execution
						argc = f.arity
						b0 = bstack
						p = loc
				}
			case ProceedInstruction() =>
				p = cp
			case CallAllocateInstruction( n ) =>
				estack = new Frame( estack, cp, n, bstack )
				regs(1) = estack.perm
			case AllocateInstruction( n ) =>
				estack = new Frame( estack, cp, n, b0 )
				regs(1) = estack.perm
			case DeallocateInstruction =>
				p = estack.cp
				estack = estack.prev
				
				if (estack eq null)
					regs(1) = null
				else
					regs(1) = estack.perm
			case TryMeElseInstruction( t ) =>
				bstack = new Choice( bstack, x, argc, estack, cp, p + t.offset, trail.size, h, callcode.size )
				hb = h
			case RetryMeElseInstruction( t ) =>
				bstack.restore
				estack = bstack.estack
				regs(1) = estack.perm
				cp = bstack.cp
				bstack.bp = p + t.offset
				unwind( bstack.tr )
				trim
				hb = h
			case TrustMeInstruction() =>
				bstack.restore
				estack = bstack.estack
				regs(1) = estack.perm
				cp = bstack.cp
				unwind( bstack.tr )
				trim
				hb = h	// this may be wrong, pg. 57 wambook.pdf
				bstack = bstack.prev
			case PutConstantInstruction( c, i ) =>
				put( x, i, ConCell(c) )
			case GetConstantInstruction( c, i ) =>
				val addr = deref( x, i )
				
				addr.read match
				{
					case RefCell( _ ) =>
						addr write ConCell( c )
						trail( addr.asInstanceOf[Addr] )
					case ConCell( const ) =>
						if (const != c)
							backtrack
					case _ => backtrack
				}
			case SetConstantInstruction( c ) =>
				setConstant( c )
			case UnifyConstantInstruction( c ) =>
				mode match
				{
					case ReadMode =>
						val addr = deref( s )
						
						addr.read match
						{
							case RefCell( _ ) =>
								addr write ConCell( c )
								trail( addr.asInstanceOf[Addr] )
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
					case RefCell( _ ) =>
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
					put( h, RefCell(h) )
					h += 1
				}
			case UnifyVoidInstruction( n ) =>
				mode match
				{
					case ReadMode => s += n
					case WriteMode =>
						for (i <- 0 until n)
						{
							put( h, RefCell(h) )
							h += 1
						}
				}
			case PutVoidInstruction( i ) =>	// not sure about this; this is to handle the rare case of _ in goal argument position
				put( h, RefCell(h) )
				put( x, i, h.read )
				h += 1
			case PutRefInstruction( a, i ) =>		// not sure about this; this is to handle variable in runtime compiled goal argument position
				put( x, i, RefCell(a) )
			case SetRefInstruction( a ) =>
				put( h, RefCell(a) )
				h += 1
			case NeckCutInstruction =>
				if (bstack ne b0)
				{
					bstack = b0
				}
			case GetLevelInstruction( n ) =>	// this is being done by 'allocate'
//				estack.perm(n) = bstack
//				estack.b0 = bstack
			case CutInstruction/*( n )*/ =>
				if (bstack ne estack.b0)
				{
					bstack = estack.b0
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
	
	protected def setConstant( c: Any ) =
	{
	val a = h
	
		put( h, ConCell(c) )
		h += 1
		a
	}
	
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
		while (trail.size > size)
		{
		val a = trail.pop
		
			a write RefCell( a )
		}
	}
	
	protected def trail( a: Addr )
	{
		trail push a
	}

	protected def bind( a1: Address, a2: Address )
	{
		if (unbound( a1 ))
		{
			put( a1, a2.read )
			trail( a1.asInstanceOf[Addr] )
		}
		else if (unbound( a2 ))
		{
			put( a2, a1.read )
			trail( a2.asInstanceOf[Addr] )
		}
		else
		{
			sys.error( "neither address is unbound" )
		}
	}
	
	protected def unify( a1: Address, a2: Address ) =
	{
	var matches = true
	
		pdl.clear
		pdl push a1
		pdl push a2
		
		while (!pdl.isEmpty && matches)
		{
		val d1 = deref( pdl pop )
		val d2 = deref( pdl pop )
		
			if (d1 != d2)
				(d1.read, d2.read) match
				{
					case (RefCell( _ ), _)|(_, RefCell( _ )) => bind( d1, d2 )
					case (ConCell( c1 ), ConCell( c2 )) => matches = c1 == c2 && c1.getClass == c2.getClass		// in Prolog 1 \= 1.0
					case (StrCell( v1: Addr ), StrCell( v2: Addr )) =>
						val f1@FunCell( _, n ) = v1.read
						val f2 = v2.read
					
						if (f1 == f2)
							for (i <- 1 to n)
							{
								pdl push (v1 + i)
								pdl push (v2 + i)
							}
						else
							matches = false
					case (LisCell( v1 ), LisCell( v2 )) =>
						pdl push v1
						pdl push v2
						pdl push (v1 + 1)
						pdl push (v2 + 1)
					case _ => matches = false
				}
		}
		
		matches
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
case class ProceedInstruction() extends Instruction	// a proceed instruction call have a label
case class CallAllocateInstruction( n: Int ) extends Instruction
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
case class PutVoidInstruction( i: Int ) extends Instruction
case class TryMeElseInstruction( t: Label ) extends Instruction
case class RetryMeElseInstruction( t: Label ) extends Instruction
case class TrustMeInstruction() extends Instruction
case class PutRefInstruction( a: Addr, i: Int ) extends Instruction
case class SetRefInstruction( a: Addr ) extends Instruction
case object NeckCutInstruction extends Instruction
case class GetLevelInstruction( n: Int ) extends Instruction
case object CutInstruction/*( n: Int )*/ extends Instruction

trait Address
{
	def read: Cell

	def write( c: Cell )
	
	def +( inc: Int ): Addr
}

class Addr( val store: Store, val ind: Int ) extends AST with Address with Ordered[Addr]
{
	def read = store(ind)

	def write( c: Cell ) = store(ind) = c
	
	val pos = null
	
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
	
	override def toString = store.name + ind
}

trait Cell extends Address
{
	def read = this

	def write( c: Cell ) = sys.error( "a Cell is read only" )
	
	def +( inc: Int ): Addr = sys.error( "a Cell is not a store address" )
}

case class RefCell( k: Addr ) extends Cell
	{
	override def read = sys.error( "RefCell can only exist in a store" )
	}
case class StrCell( k: Addr ) extends Cell
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

class Frame( val prev: Frame, val cp: Int, n: Int, val b0: Choice )
{
	val perm = new Store( "Y", n )
}

class Choice( val prev: Choice, regs: Store, n: Int, val estack: Frame, val cp: Int, var bp: Int, val tr: Int, val h: Addr, val callcode: Int )
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

abstract class WAMInterface( val wam: WAM )
{
	val estack: Frame
	val bstack: Choice
	val x: Store
	
	def backtrack
	
	def unwind( size: Int )
	
	def trail( a: Addr )
	
	def bind( a1: Address, a2: Address )
	
	def unify( a1: Address, a2: Address ): Boolean
}