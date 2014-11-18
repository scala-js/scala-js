package java.nio

object ByteBuffer {
  private final val HashSeed = -547316498 // "java.nio.ByteBuffer".##

  def allocate(capacity: Int): ByteBuffer =
    wrap(new Array[Byte](capacity))

  def allocateDirect(capacity: Int): ByteBuffer =
    allocate(capacity)

  def wrap(array: Array[Byte], offset: Int, length: Int): ByteBuffer =
    HeapByteBuffer.wrap(array, 0, array.length, offset, length, false)

  def wrap(array: Array[Byte]): ByteBuffer =
    wrap(array, 0, array.length)
}

abstract class ByteBuffer private[nio] (
    _capacity: Int, private[nio] val _array: Array[Byte],
    private[nio] val _arrayOffset: Int)
    extends Buffer(_capacity) with Comparable[ByteBuffer] {

  def this(_capacity: Int) = this(_capacity, null, -1)

  private var _order: ByteOrder = ByteOrder.BIG_ENDIAN

  def slice(): ByteBuffer

  def duplicate(): ByteBuffer

  def asReadOnlyBuffer(): ByteBuffer

  def get(): Byte

  def put(b: Byte): ByteBuffer

  def get(index: Int): Byte

  def put(index: Int, b: Byte): ByteBuffer

  def get(dst: Array[Byte], offset: Int, length: Int): ByteBuffer = {
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

  def get(dst: Array[Byte]): ByteBuffer =
    get(dst, 0, dst.length)

  def put(src: ByteBuffer): ByteBuffer = {
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

  def put(src: Array[Byte], offset: Int, length: Int): ByteBuffer = {
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

  final def put(src: Array[Byte]): ByteBuffer =
    put(src, 0, src.length)

  @inline final def hasArray(): Boolean = _array != null && !isReadOnly

  @inline final def array(): Array[Byte] = {
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

  def compact(): ByteBuffer

  // Not implemented:
  //def isDirect(): Boolean

  // toString(): String inherited from Buffer

  override def hashCode(): Int = {
    import scala.util.hashing.MurmurHash3._
    val start = position
    val end = limit
    var h = ByteBuffer.HashSeed
    var i = start
    while (i != end) {
      h = mix(h, get().##)
      i += 1
    }
    position(start)
    finalizeHash(h, end-start)
  }

  override def equals(that: Any): Boolean = that match {
    case that: ByteBuffer => compareTo(that) == 0
    case _                => false
  }

  def compareTo(that: ByteBuffer): Int = {
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

  final def order(): ByteOrder = _order

  final def order(bo: ByteOrder): ByteBuffer = {
    if (bo == null)
      throw new NullPointerException
    _order = bo
    this
  }

  /* Not implemented:

  def getChar(): Char
  def putChar(value: Char): ByteBuffer
  def getChar(index: Int): Char
  def putChar(index: Int, value: Char): ByteBuffer
  def asCharBuffer(): CharBuffer

  def getShort(): Short
  def putShort(value: Short): ByteBuffer
  def getShort(index: Int): Short
  def putShort(index: Int, value: Short): ByteBuffer
  def asShortBuffer(): ShortBuffer

  def getInt(): Int
  def putInt(value: Int): ByteBuffer
  def getInt(index: Int): Int
  def putInt(index: Int, value: Int): ByteBuffer
  def asIntBuffer(): IntBuffer

  def getLong(): Long
  def putLong(value: Long): ByteBuffer
  def getLong(index: Int): Long
  def putLong(index: Int, value: Long): ByteBuffer
  def asLongBuffer(): LongBuffer

  def getFloat(): Float
  def putFloat(value: Float): ByteBuffer
  def getFloat(index: Int): Float
  def putFloat(index: Int, value: Float): ByteBuffer
  def asFloatBuffer(): FloatBuffer

  def getDouble(): Double
  def putDouble(value: Double): ByteBuffer
  def getDouble(index: Int): Double
  def putDouble(index: Int, value: Double): ByteBuffer
  def asDoubleBuffer(): DoubleBuffer

  */
}
