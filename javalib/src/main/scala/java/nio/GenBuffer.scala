package java.nio

private[nio] object GenBuffer {
  def apply[B <: Buffer](self: B): GenBuffer[B] =
    new GenBuffer(self)
}

private[nio] final class GenBuffer[B <: Buffer](val self: B) extends AnyVal {
  import self._

  @inline
  def generic_get(dst: Array[ElementType],
      offset: Int, length: Int): BufferType = {
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

    self
  }

  @inline
  def generic_put(src: BufferType): BufferType = {
    if (src eq self)
      throw new IllegalArgumentException
    if (isReadOnly)
      throw new ReadOnlyBufferException
    if (src.remaining > remaining)
      throw new BufferOverflowException

    var n = src.remaining
    val srcArray = src._array // even if read-only
    if (srcArray != null) {
      val pos = src.position
      put(srcArray, src._arrayOffset + pos, n)
      src.position(pos + n)
    } else {
      while (n != 0) {
        put(src.get())
        n -= 1
      }
    }

    self
  }

  @inline
  def generic_put(src: Array[ElementType],
      offset: Int, length: Int): BufferType = {
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

    self
  }

  @inline
  def generic_hasArray(): Boolean =
    _array != null && !isReadOnly

  @inline
  def generic_array(): Array[ElementType] = {
    val a = _array
    if (a == null)
      throw new UnsupportedOperationException
    if (isReadOnly)
      throw new ReadOnlyBufferException
    a
  }

  @inline
  def generic_arrayOffset(): Int = {
    val o = _arrayOffset
    if (o == -1)
      throw new UnsupportedOperationException
    if (isReadOnly)
      throw new ReadOnlyBufferException
    o
  }

  @inline
  def generic_hashCode(hashSeed: Int): Int = {
    import scala.util.hashing.MurmurHash3._
    val start = position
    val end = limit
    var h = hashSeed
    var i = start
    while (i != end) {
      h = mix(h, get().##)
      i += 1
    }
    position(start)
    finalizeHash(h, end-start)
  }

  @inline
  def generic_compareTo(that: BufferType)(
      compare: (ElementType, ElementType) => Int): Int = {
    if (self eq that) {
      0
    } else {
      val thisStart = self.position
      val thisRemaining = self.remaining
      val thatStart = that.position
      val thatRemaining = that.remaining
      val shortestLength = Math.min(thisRemaining, thatRemaining)

      var i = 0
      while (i != shortestLength) {
        val cmp = compare(self.get(), that.get())
        if (cmp != 0) {
          self.position(thisStart)
          that.position(thatStart)
          return cmp
        }
        i += 1
      }

      self.position(thisStart)
      that.position(thatStart)
      thisRemaining.compareTo(thatRemaining)
    }
  }

}
