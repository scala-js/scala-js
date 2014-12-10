package java.nio

private[nio] final class StringCharBuffer private (
    _capacity: Int, _csq: CharSequence, _csqOffset: Int,
    _initialPosition: Int, _initialLimit: Int)
    extends CharBuffer(_capacity) {

  position(_initialPosition)
  limit(_initialLimit)

  def isReadOnly(): Boolean = true

  def isDirect(): Boolean = false

  def slice(): CharBuffer = {
    val cap = remaining
    new StringCharBuffer(cap, _csq, _csqOffset + position, 0, cap)
  }

  def duplicate(): CharBuffer = {
    val result = new StringCharBuffer(capacity, _csq, _csqOffset,
        position, limit)
    result._mark = this._mark
    result
  }

  def asReadOnlyBuffer(): CharBuffer = duplicate()

  def subSequence(start: Int, end: Int): CharBuffer = {
    if (start < 0 || end < start || end > remaining)
      throw new IndexOutOfBoundsException
    new StringCharBuffer(capacity, _csq, _csqOffset,
        position + start, position + end)
  }

  def get(): Char = {
    if (!hasRemaining)
      throw new BufferUnderflowException
    val p = position
    position(p + 1)
    _csq.charAt(_csqOffset + p)
  }

  def put(c: Char): CharBuffer =
    throw new ReadOnlyBufferException

  def get(index: Int): Char = {
    if (index < 0 || index >= limit)
      throw new IndexOutOfBoundsException
    _csq.charAt(_csqOffset + index)
  }

  def put(index: Int, b: Char): CharBuffer =
    throw new ReadOnlyBufferException

  override def get(dst: Array[Char], offset: Int, length: Int): CharBuffer = {
    val end = offset + length

    if (offset < 0 || length < 0 || end > dst.length)
      throw new IndexOutOfBoundsException

    val startPos = position
    val endPos = startPos + length
    if (endPos > limit)
      throw new BufferUnderflowException

    var i = offset
    var j = startPos + _csqOffset
    while (i != end) {
      dst(i) = _csq.charAt(j)
      i += 1
      j += 1
    }
    position(endPos)

    this
  }

  def compact(): CharBuffer =
    throw new ReadOnlyBufferException

  override def toString(): String = {
    val offset = _csqOffset
    _csq.subSequence(position + offset, limit + offset).toString()
  }

  def order(): ByteOrder = ByteOrder.nativeOrder()
}

private[nio] object StringCharBuffer {
  private[nio] def wrap(csq: CharSequence, csqOffset: Int, capacity: Int,
      initialPosition: Int, initialLength: Int): CharBuffer = {
    if (csqOffset < 0 || capacity < 0 || csqOffset+capacity > csq.length)
      throw new IndexOutOfBoundsException
    val initialLimit = initialPosition + initialLength
    if (initialPosition < 0 || initialLength < 0 || initialLimit > capacity)
      throw new IndexOutOfBoundsException
    new StringCharBuffer(capacity, csq, csqOffset,
        initialPosition, initialLimit)
  }
}
