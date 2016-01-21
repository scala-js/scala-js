package java.nio

import scala.scalajs.js.typedarray._

object CharBuffer {
  private final val HashSeed = -182887236 // "java.nio.CharBuffer".##

  def allocate(capacity: Int): CharBuffer =
    wrap(new Array[Char](capacity))

  def wrap(array: Array[Char], offset: Int, length: Int): CharBuffer =
    HeapCharBuffer.wrap(array, 0, array.length, offset, length, false)

  def wrap(array: Array[Char]): CharBuffer =
    wrap(array, 0, array.length)

  def wrap(csq: CharSequence, start: Int, end: Int): CharBuffer =
    StringCharBuffer.wrap(csq, 0, csq.length, start, end - start)

  def wrap(csq: CharSequence): CharBuffer =
    wrap(csq, 0, csq.length)

  // Extended API

  def wrap(array: Uint16Array): CharBuffer =
    TypedArrayCharBuffer.wrap(array)
}

abstract class CharBuffer private[nio] (
    _capacity: Int, private[nio] val _array: Array[Char],
    private[nio] val _arrayOffset: Int)
    extends Buffer(_capacity) with Comparable[CharBuffer]
                              with CharSequence with Appendable with Readable {

  private[nio] type ElementType = Char
  private[nio] type BufferType = CharBuffer
  private[nio] type TypedArrayType = Uint16Array

  def this(_capacity: Int) = this(_capacity, null, -1)

  def read(target: CharBuffer): Int = {
    // Attention: this method must not change this buffer's position
    val n = remaining
    if (n == 0) -1
    else if (_array != null) { // even if read-only
      target.put(_array, _arrayOffset, n)
      n
    } else {
      val savedPos = position
      target.put(this)
      position(savedPos)
      n
    }
  }

  def slice(): CharBuffer

  def duplicate(): CharBuffer

  def asReadOnlyBuffer(): CharBuffer

  def get(): Char

  def put(c: Char): CharBuffer

  def get(index: Int): Char

  def put(index: Int, c: Char): CharBuffer

  @noinline
  def get(dst: Array[Char], offset: Int, length: Int): CharBuffer =
    GenBuffer(this).generic_get(dst, offset, length)

  def get(dst: Array[Char]): CharBuffer =
    get(dst, 0, dst.length)

  @noinline
  def put(src: CharBuffer): CharBuffer =
    GenBuffer(this).generic_put(src)

  @noinline
  def put(src: Array[Char], offset: Int, length: Int): CharBuffer =
    GenBuffer(this).generic_put(src, offset, length)

  final def put(src: Array[Char]): CharBuffer =
    put(src, 0, src.length)

  def put(src: String, start: Int, end: Int): CharBuffer =
    put(CharBuffer.wrap(src, start, end))

  final def put(src: String): CharBuffer =
    put(src, 0, src.length)

  @inline final def hasArray(): Boolean =
    GenBuffer(this).generic_hasArray()

  @inline final def array(): Array[Char] =
    GenBuffer(this).generic_array()

  @inline final def arrayOffset(): Int =
    GenBuffer(this).generic_arrayOffset()

  def compact(): CharBuffer

  def isDirect(): Boolean

  @noinline
  override def hashCode(): Int =
    GenBuffer(this).generic_hashCode(CharBuffer.HashSeed)

  override def equals(that: Any): Boolean = that match {
    case that: CharBuffer => compareTo(that) == 0
    case _                => false
  }

  @noinline
  def compareTo(that: CharBuffer): Int =
    GenBuffer(this).generic_compareTo(that)(_.compareTo(_))

  override def toString(): String = {
    if (_array != null) { // even if read-only
      new String(_array, position + _arrayOffset, remaining)
    } else {
      val chars = new Array[Char](remaining)
      val savedPos = position
      get(chars)
      position(savedPos)
      new String(chars)
    }
  }

  final def length(): Int = remaining

  final def charAt(index: Int): Char = get(position + index)

  def subSequence(start: Int, end: Int): CharSequence

  def append(csq: CharSequence): CharBuffer =
    put(csq.toString())

  def append(csq: CharSequence, start: Int, end: Int): CharBuffer =
    put(csq.subSequence(start, end).toString())

  def append(c: Char): CharBuffer =
    put(c)

  def order(): ByteOrder

  // Internal API

  private[nio] def load(index: Int): Char

  private[nio] def store(index: Int, elem: Char): Unit

  @inline
  private[nio] def load(startIndex: Int,
      dst: Array[Char], offset: Int, length: Int): Unit =
    GenBuffer(this).generic_load(startIndex, dst, offset, length)

  @inline
  private[nio] def store(startIndex: Int,
      src: Array[Char], offset: Int, length: Int): Unit =
    GenBuffer(this).generic_store(startIndex, src, offset, length)
}
