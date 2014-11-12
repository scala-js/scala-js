package java.nio

private[nio] final class HeapByteBuffer private (
    _capacity: Int, _array0: Array[Byte], _arrayOffset0: Int,
    _initialPosition: Int, _initialLimit: Int, _readOnly: Boolean)
    extends ByteBuffer(_capacity, _array0, _arrayOffset0) {

  position(_initialPosition)
  limit(_initialLimit)

  def isReadOnly(): Boolean = _readOnly

  def isDirect(): Boolean = false

  def slice(): ByteBuffer = {
    val cap = remaining
    new HeapByteBuffer(cap, _array, _arrayOffset+position, 0, cap, isReadOnly)
  }

  def duplicate(): ByteBuffer = {
    val result = new HeapByteBuffer(capacity, _array, _arrayOffset,
        position, limit, isReadOnly)
    result._mark = this._mark
    result
  }

  def asReadOnlyBuffer(): ByteBuffer = {
    val result = new HeapByteBuffer(capacity, _array, _arrayOffset,
        position, limit, true)
    result._mark = this._mark
    result
  }

  def get(): Byte = {
    if (!hasRemaining)
      throw new BufferUnderflowException
    val p = position
    position(p + 1)
    _array(_arrayOffset + p)
  }

  def put(b: Byte): ByteBuffer = {
    if (isReadOnly)
      throw new ReadOnlyBufferException
    if (!hasRemaining)
      throw new BufferOverflowException
    val p = position
    _array(_arrayOffset + p) = b
    position(p + 1)
    this
  }

  def get(index: Int): Byte = {
    if (index < 0 || index >= limit)
      throw new IndexOutOfBoundsException
    _array(_arrayOffset + index)
  }

  def put(index: Int, b: Byte): ByteBuffer = {
    if (isReadOnly)
      throw new ReadOnlyBufferException
    if (index < 0 || index >= limit)
      throw new IndexOutOfBoundsException
    _array(_arrayOffset + index) = b
    this
  }

  override def get(dst: Array[Byte], offset: Int, length: Int): ByteBuffer = {
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

  override def put(src: Array[Byte], offset: Int, length: Int): ByteBuffer = {
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

  def compact(): ByteBuffer = {
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
}

private[nio] object HeapByteBuffer {
  private[nio] def wrap(array: Array[Byte], arrayOffset: Int, capacity: Int,
      initialPosition: Int, initialLength: Int,
      isReadOnly: Boolean): ByteBuffer = {
    if (arrayOffset < 0 || capacity < 0 || arrayOffset+capacity > array.length)
      throw new IndexOutOfBoundsException
    val initialLimit = initialPosition + initialLength
    if (initialPosition < 0 || initialLength < 0 || initialLimit > capacity)
      throw new IndexOutOfBoundsException
    new HeapByteBuffer(capacity, array, arrayOffset,
        initialPosition, initialLimit, isReadOnly)
  }
}
