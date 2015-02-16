package java.nio

private[nio] object GenHeapBuffer {
  def apply[B <: Buffer](self: B): GenHeapBuffer[B] =
    new GenHeapBuffer(self)

  trait NewHeapBuffer[BufferType <: Buffer, ElementType] {
    def apply(capacity: Int, array: Array[ElementType], arrayOffset: Int,
        initialPosition: Int, initialLimit: Int, readOnly: Boolean): BufferType
  }

  @inline
  def generic_wrap[BufferType <: Buffer, ElementType](
      array: Array[ElementType], arrayOffset: Int, capacity: Int,
      initialPosition: Int, initialLength: Int, isReadOnly: Boolean)(
      implicit newHeapBuffer: NewHeapBuffer[BufferType, ElementType]): BufferType = {
    if (arrayOffset < 0 || capacity < 0 || arrayOffset+capacity > array.length)
      throw new IndexOutOfBoundsException
    val initialLimit = initialPosition + initialLength
    if (initialPosition < 0 || initialLength < 0 || initialLimit > capacity)
      throw new IndexOutOfBoundsException
    newHeapBuffer(capacity, array, arrayOffset,
        initialPosition, initialLimit, isReadOnly)
  }
}

private[nio] final class GenHeapBuffer[B <: Buffer](val self: B) extends AnyVal {
  import self._

  type NewThisHeapBuffer = GenHeapBuffer.NewHeapBuffer[BufferType, ElementType]

  @inline
  def generic_slice()(
      implicit newHeapBuffer: NewThisHeapBuffer): BufferType = {
    val newCapacity = remaining
    newHeapBuffer(newCapacity, _array, _arrayOffset + position,
        0, newCapacity, isReadOnly)
  }

  @inline
  def generic_duplicate()(
      implicit newHeapBuffer: NewThisHeapBuffer): BufferType = {
    val result = newHeapBuffer(capacity, _array, _arrayOffset,
        position, limit, isReadOnly)
    result._mark = _mark
    result
  }

  @inline
  def generic_asReadOnlyBuffer()(
      implicit newHeapBuffer: NewThisHeapBuffer): BufferType = {
    val result = newHeapBuffer(capacity, _array, _arrayOffset,
        position, limit, true)
    result._mark = _mark
    result
  }

  @inline
  def generic_get(): ElementType = {
    if (!hasRemaining)
      throw new BufferUnderflowException
    val p = position
    position(p + 1)
    _array(_arrayOffset + p)
  }

  @inline
  def generic_put(elem: ElementType): B = {
    if (isReadOnly)
      throw new ReadOnlyBufferException
    if (!hasRemaining)
      throw new BufferOverflowException
    val p = position
    _array(_arrayOffset + p) = elem
    position(p + 1)
    self
  }

  @inline
  def generic_get(index: Int): ElementType = {
    if (index < 0 || index >= limit)
      throw new IndexOutOfBoundsException
    _array(_arrayOffset + index)
  }

  @inline
  def generic_put(index: Int, elem: ElementType): BufferType = {
    if (isReadOnly)
      throw new ReadOnlyBufferException
    if (index < 0 || index >= limit)
      throw new IndexOutOfBoundsException
    _array(_arrayOffset + index) = elem
    self
  }

  @inline
  def generic_get(dst: Array[ElementType],
      offset: Int, length: Int): BufferType = {
    val end = offset + length

    if (offset < 0 || length < 0 || end > dst.length)
      throw new IndexOutOfBoundsException

    val startPos = position
    val endPos = startPos + length
    if (endPos > limit)
      throw new BufferUnderflowException

    System.arraycopy(_array, startPos + _arrayOffset, dst, offset, length)
    position(endPos)

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

    val startPos = position
    val endPos = startPos + length
    if (endPos > limit)
      throw new BufferOverflowException

    System.arraycopy(src, offset, _array, startPos + _arrayOffset, length)
    position(endPos)

    self
  }

  @inline
  def generic_compact(): BufferType = {
    if (isReadOnly)
      throw new ReadOnlyBufferException

    val len = remaining
    System.arraycopy(_array, _arrayOffset + position, _array, _arrayOffset, len)
    _mark = -1
    limit(capacity)
    position(len)
    self
  }

}
