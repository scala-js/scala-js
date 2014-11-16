package java.nio

object CharBuffer {
  private final val HashSeed = -182887236 // "java.nio.CharBuffer".##

  def allocate(capacity: Int): CharBuffer =
    wrap(new Array[Char](capacity))

  def wrap(array: Array[Char], offset: Int, length: Int): CharBuffer =
    HeapCharBuffer.wrap(array, 0, array.length, offset, length, false)

  def wrap(array: Array[Char]): CharBuffer =
    wrap(array, 0, array.length)

  def wrap(csq: CharSequence, start: Int, end: Int): CharBuffer =
    StringCharBuffer.wrap(csq, 0, csq.length, start, end)

  def wrap(csq: CharSequence): CharBuffer =
    wrap(csq, 0, csq.length)
}

abstract class CharBuffer private[nio] (
    _capacity: Int, private[nio] val _array: Array[Char],
    private[nio] val _arrayOffset: Int)
    extends Buffer(_capacity) with Comparable[CharBuffer]
                              with CharSequence with Appendable with Readable {

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

  def get(dst: Array[Char], offset: Int, length: Int): CharBuffer = {
    val end = offset + length

    if (offset < 0 || length < 0 || end > dst.length)
      throw new IndexOutOfBoundsException
    if (remaining < length)
      throw new BufferUnderflowException

    var i = offset
    while (i != end) {
      dst(i) = get()
      i += 1
    }

    this
  }

  def get(dst: Array[Char]): CharBuffer =
    get(dst, 0, dst.length)

  def put(src: CharBuffer): CharBuffer = {
    if (src eq this)
      throw new IllegalArgumentException
    if (isReadOnly)
      throw new ReadOnlyBufferException
    if (src.remaining > remaining)
      throw new BufferOverflowException

    var n = src.remaining
    if (src._array != null) { // even if read-only
      val pos = src.position
      put(src._array, src._arrayOffset + pos, n)
      src.position(pos + n)
    } else {
      while (n != 0) {
        put(src.get())
        n -= 1
      }
    }

    this
  }

  def put(src: Array[Char], offset: Int, length: Int): CharBuffer = {
    val end = offset + length
    if (offset < 0 || length < 0 || end > src.length)
      throw new IndexOutOfBoundsException
    if (isReadOnly)
      throw new ReadOnlyBufferException
    if (remaining < length)
      throw new BufferOverflowException

    var i = offset
    while (i != end) {
      put(src(i))
      i += 1
    }

    this
  }

  final def put(src: Array[Char]): CharBuffer =
    put(src, 0, src.length)

  def put(src: String, start: Int, end: Int): CharBuffer =
    put(CharBuffer.wrap(src, start, end))

  final def put(src: String): CharBuffer =
    put(src, 0, src.length)

  @inline final def hasArray(): Boolean = _array != null && !isReadOnly

  @inline final def array(): Array[Char] = {
    val a = _array
    if (a == null)
      throw new UnsupportedOperationException
    if (isReadOnly)
      throw new ReadOnlyBufferException
    a
  }

  @inline final def arrayOffset(): Int = {
    val o = _arrayOffset
    if (o == -1)
      throw new UnsupportedOperationException
    if (isReadOnly)
      throw new ReadOnlyBufferException
    o
  }

  def compact(): CharBuffer

  // Not implemented:
  //def isDirect(): Boolean

  override def hashCode(): Int = {
    import scala.util.hashing.MurmurHash3._
    val start = position
    val end = limit
    var h = CharBuffer.HashSeed
    var i = start
    while (i != end) {
      h = mix(h, get().##)
      i += 1
    }
    position(start)
    finalizeHash(h, end-start)
  }

  override def equals(that: Any): Boolean = that match {
    case that: CharBuffer => compareTo(that) == 0
    case _                => false
  }

  def compareTo(that: CharBuffer): Int = {
    if (this eq that) {
      0
    } else {
      val thisStart = this.position
      val thisRemaining = this.remaining
      val thatStart = that.position
      val thatRemaining = that.remaining
      val shortestLength = Math.min(thisRemaining, thatRemaining)

      var i = 0
      while (i != shortestLength) {
        val cmp = this.get().compareTo(that.get())
        if (cmp != 0) {
          this.position(thisStart)
          that.position(thatStart)
          return cmp
        }
        i += 1
      }

      this.position(thisStart)
      that.position(thatStart)
      thisRemaining.compareTo(thatRemaining)
    }
  }

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
}
