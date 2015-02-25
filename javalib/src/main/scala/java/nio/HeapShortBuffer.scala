package java.nio

private[nio] final class HeapShortBuffer private (
    _capacity: Int, _array0: Array[Short], _arrayOffset0: Int,
    _initialPosition: Int, _initialLimit: Int, _readOnly: Boolean)
    extends ShortBuffer(_capacity, _array0, _arrayOffset0) {

  position(_initialPosition)
  limit(_initialLimit)

  private[this] implicit def newHeapShortBuffer = HeapShortBuffer.NewHeapShortBuffer

  def isReadOnly(): Boolean = _readOnly

  def isDirect(): Boolean = false

  @noinline
  def slice(): ShortBuffer =
    GenHeapBuffer(this).generic_slice()

  @noinline
  def duplicate(): ShortBuffer =
    GenHeapBuffer(this).generic_duplicate()

  @noinline
  def asReadOnlyBuffer(): ShortBuffer =
    GenHeapBuffer(this).generic_asReadOnlyBuffer()

  @noinline
  def get(): Short =
    GenBuffer(this).generic_get()

  @noinline
  def put(s: Short): ShortBuffer =
    GenBuffer(this).generic_put(s)

  @noinline
  def get(index: Int): Short =
    GenBuffer(this).generic_get(index)

  @noinline
  def put(index: Int, s: Short): ShortBuffer =
    GenBuffer(this).generic_put(index, s)

  @noinline
  override def get(dst: Array[Short], offset: Int, length: Int): ShortBuffer =
    GenBuffer(this).generic_get(dst, offset, length)

  @noinline
  override def put(src: Array[Short], offset: Int, length: Int): ShortBuffer =
    GenBuffer(this).generic_put(src, offset, length)

  @noinline
  def compact(): ShortBuffer =
    GenHeapBuffer(this).generic_compact()

  def order(): ByteOrder = ByteOrder.nativeOrder()

  // Internal API

  @inline
  private[nio] def load(index: Int): Short =
    GenHeapBuffer(this).generic_load(index)

  @inline
  private[nio] def store(index: Int, elem: Short): Unit =
    GenHeapBuffer(this).generic_store(index, elem)

  @inline
  override private[nio] def load(startIndex: Int,
      dst: Array[Short], offset: Int, length: Int): Unit =
    GenHeapBuffer(this).generic_load(startIndex, dst, offset, length)

  @inline
  override private[nio] def store(startIndex: Int,
      src: Array[Short], offset: Int, length: Int): Unit =
    GenHeapBuffer(this).generic_store(startIndex, src, offset, length)
}

private[nio] object HeapShortBuffer {
  private[nio] implicit object NewHeapShortBuffer
      extends GenHeapBuffer.NewHeapBuffer[ShortBuffer, Short] {
    def apply(capacity: Int, array: Array[Short], arrayOffset: Int,
        initialPosition: Int, initialLimit: Int,
        readOnly: Boolean): ShortBuffer = {
      new HeapShortBuffer(capacity, array, arrayOffset,
          initialPosition, initialLimit, readOnly)
    }
  }

  @noinline
  private[nio] def wrap(array: Array[Short], arrayOffset: Int, capacity: Int,
      initialPosition: Int, initialLength: Int,
      isReadOnly: Boolean): ShortBuffer = {
    GenHeapBuffer.generic_wrap(
        array, arrayOffset, capacity,
        initialPosition, initialLength, isReadOnly)
  }
}
