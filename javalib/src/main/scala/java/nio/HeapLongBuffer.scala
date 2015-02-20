package java.nio

private[nio] final class HeapLongBuffer private (
    _capacity: Int, _array0: Array[Long], _arrayOffset0: Int,
    _initialPosition: Int, _initialLimit: Int, _readOnly: Boolean)
    extends LongBuffer(_capacity, _array0, _arrayOffset0) {

  position(_initialPosition)
  limit(_initialLimit)

  private[this] implicit def newHeapLongBuffer = HeapLongBuffer.NewHeapLongBuffer

  def isReadOnly(): Boolean = _readOnly

  def isDirect(): Boolean = false

  @noinline
  def slice(): LongBuffer =
    GenHeapBuffer(this).generic_slice()

  @noinline
  def duplicate(): LongBuffer =
    GenHeapBuffer(this).generic_duplicate()

  @noinline
  def asReadOnlyBuffer(): LongBuffer =
    GenHeapBuffer(this).generic_asReadOnlyBuffer()

  @noinline
  def get(): Long =
    GenBuffer(this).generic_get()

  @noinline
  def put(l: Long): LongBuffer =
    GenBuffer(this).generic_put(l)

  @noinline
  def get(index: Int): Long =
    GenBuffer(this).generic_get(index)

  @noinline
  def put(index: Int, l: Long): LongBuffer =
    GenBuffer(this).generic_put(index, l)

  @noinline
  override def get(dst: Array[Long], offset: Int, length: Int): LongBuffer =
    GenBuffer(this).generic_get(dst, offset, length)

  @noinline
  override def put(src: Array[Long], offset: Int, length: Int): LongBuffer =
    GenBuffer(this).generic_put(src, offset, length)

  @noinline
  def compact(): LongBuffer =
    GenHeapBuffer(this).generic_compact()

  def order(): ByteOrder = ByteOrder.nativeOrder()

  // Internal API

  @inline
  private[nio] def load(index: Int): Long =
    GenHeapBuffer(this).generic_load(index)

  @inline
  private[nio] def store(index: Int, elem: Long): Unit =
    GenHeapBuffer(this).generic_store(index, elem)

  @inline
  override private[nio] def load(startIndex: Int,
      dst: Array[Long], offset: Int, length: Int): Unit =
    GenHeapBuffer(this).generic_load(startIndex, dst, offset, length)

  @inline
  override private[nio] def store(startIndex: Int,
      src: Array[Long], offset: Int, length: Int): Unit =
    GenHeapBuffer(this).generic_store(startIndex, src, offset, length)
}

private[nio] object HeapLongBuffer {
  private[nio] implicit object NewHeapLongBuffer
      extends GenHeapBuffer.NewHeapBuffer[LongBuffer, Long] {
    def apply(capacity: Int, array: Array[Long], arrayOffset: Int,
        initialPosition: Int, initialLimit: Int,
        readOnly: Boolean): LongBuffer = {
      new HeapLongBuffer(capacity, array, arrayOffset,
          initialPosition, initialLimit, readOnly)
    }
  }

  @noinline
  private[nio] def wrap(array: Array[Long], arrayOffset: Int, capacity: Int,
      initialPosition: Int, initialLength: Int,
      isReadOnly: Boolean): LongBuffer = {
    GenHeapBuffer.generic_wrap(
        array, arrayOffset, capacity,
        initialPosition, initialLength, isReadOnly)
  }
}
