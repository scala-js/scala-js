package java.nio

private[nio] final class HeapCharBuffer private (
    _capacity: Int, _array0: Array[Char], _arrayOffset0: Int,
    _initialPosition: Int, _initialLimit: Int, _readOnly: Boolean)
    extends CharBuffer(_capacity, _array0, _arrayOffset0) {

  position(_initialPosition)
  limit(_initialLimit)

  def isReadOnly(): Boolean = _readOnly

  def isDirect(): Boolean = false

  def slice(): CharBuffer = {
    val cap = remaining
    new HeapCharBuffer(cap, _array, _arrayOffset + position, 0, cap, isReadOnly)
  }

  def duplicate(): CharBuffer = {
    val result = new HeapCharBuffer(capacity, _array, _arrayOffset,
        position, limit, isReadOnly)
    result._mark = this._mark
    result
  }

  def asReadOnlyBuffer(): CharBuffer = {
    val result = new HeapCharBuffer(capacity, _array, _arrayOffset,
        position, limit, true)
    result._mark = this._mark
    result
  }

  def subSequence(start: Int, end: Int): CharBuffer = {
    if (start < 0 || end < start || end > remaining)
      throw new IndexOutOfBoundsException
    new HeapCharBuffer(capacity, _array, _arrayOffset,
        position + start, position + end, isReadOnly)
  }

  def get(): Char = {
    if (!hasRemaining)
      throw new BufferUnderflowException
    val p = position
    position(p + 1)
    _array(_arrayOffset + p)
  }

  def put(c: Char): CharBuffer = {
    if (isReadOnly)
      throw new ReadOnlyBufferException
    if (!hasRemaining)
      throw new BufferOverflowException
    val p = position
    _array(_arrayOffset + p) = c
    position(p + 1)
    this
  }

  def get(index: Int): Char = {
    if (index < 0 || index >= limit)
      throw new IndexOutOfBoundsException
    _array(_arrayOffset + index)
  }

  def put(index: Int, b: Char): CharBuffer = {
    if (isReadOnly)
      throw new ReadOnlyBufferException
    if (index < 0 || index >= limit)
      throw new IndexOutOfBoundsException
    _array(_arrayOffset + index) = b
    this
  }

  override def get(dst: Array[Char], offset: Int, length: Int): CharBuffer = {
    val end = offset + length

    if (offset < 0 || length < 0 || end > dst.length)
      throw new IndexOutOfBoundsException

    val startPos = position
    val endPos = startPos + length
    if (endPos > limit)
      throw new BufferUnderflowException

    System.arraycopy(_array, startPos + _arrayOffset, dst, offset, length)
    position(endPos)

    this
  }

  override def put(src: Array[Char], offset: Int, length: Int): CharBuffer = {
    val end = offset + length
    if (offset < 0 || length < 0 || end > src.length)
      throw new IndexOutOfBoundsException
    if (isReadOnly)
      throw new ReadOnlyBufferException

    val startPos = position
    val endPos = startPos + length
    if (endPos > limit)
      throw new BufferOverflowException

    System.arraycopy(src, offset, _array, startPos + _arrayOffset, length)
    position(endPos)

    this
  }

  def compact(): CharBuffer = {
    if (isReadOnly)
      throw new ReadOnlyBufferException

    val offset = _arrayOffset
    val len = remaining
    System.arraycopy(_array, offset + position, _array, offset, len)
    _mark = -1
    limit(capacity)
    position(len)
    this
  }

  def order(): ByteOrder = ByteOrder.nativeOrder()
}

private[nio] object HeapCharBuffer {
  private[nio] def wrap(array: Array[Char], arrayOffset: Int, capacity: Int,
      initialPosition: Int, initialLength: Int,
      isReadOnly: Boolean): CharBuffer = {
    if (arrayOffset < 0 || capacity < 0 || arrayOffset+capacity > array.length)
      throw new IndexOutOfBoundsException
    val initialLimit = initialPosition + initialLength
    if (initialPosition < 0 || initialLength < 0 || initialLimit > capacity)
      throw new IndexOutOfBoundsException
    new HeapCharBuffer(capacity, array, arrayOffset,
        initialPosition, initialLimit, isReadOnly)
  }
}
