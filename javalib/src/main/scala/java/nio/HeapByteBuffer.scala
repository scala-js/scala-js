package java.nio

private[nio] final class HeapByteBuffer private (
    _capacity: Int, _array0: Array[Byte], _arrayOffset0: Int,
    _initialPosition: Int, _initialLimit: Int, _readOnly: Boolean)
    extends ByteBuffer(_capacity, _array0, _arrayOffset0) {

  position(_initialPosition)
  limit(_initialLimit)

  private[this] implicit def newHeapByteBuffer = HeapByteBuffer.NewHeapByteBuffer

  def isReadOnly(): Boolean = _readOnly

  def isDirect(): Boolean = false

  @noinline
  def slice(): ByteBuffer =
    GenHeapBuffer(this).generic_slice()

  @noinline
  def duplicate(): ByteBuffer =
    GenHeapBuffer(this).generic_duplicate()

  @noinline
  def asReadOnlyBuffer(): ByteBuffer =
    GenHeapBuffer(this).generic_asReadOnlyBuffer()

  @noinline
  def get(): Byte =
    GenHeapBuffer(this).generic_get()

  @noinline
  def put(b: Byte): ByteBuffer =
    GenHeapBuffer(this).generic_put(b)

  @noinline
  def get(index: Int): Byte =
    GenHeapBuffer(this).generic_get(index)

  @noinline
  def put(index: Int, b: Byte): ByteBuffer =
    GenHeapBuffer(this).generic_put(index, b)

  @noinline
  override def get(dst: Array[Byte], offset: Int, length: Int): ByteBuffer =
    GenHeapBuffer(this).generic_get(dst, offset, length)

  @noinline
  override def put(src: Array[Byte], offset: Int, length: Int): ByteBuffer =
    GenHeapBuffer(this).generic_put(src, offset, length)

  @noinline
  def compact(): ByteBuffer =
    GenHeapBuffer(this).generic_compact()
}

private[nio] object HeapByteBuffer {
  private[nio] implicit object NewHeapByteBuffer
      extends GenHeapBuffer.NewHeapBuffer[ByteBuffer, Byte] {
    def apply(capacity: Int, array: Array[Byte], arrayOffset: Int,
        initialPosition: Int, initialLimit: Int,
        readOnly: Boolean): ByteBuffer = {
      new HeapByteBuffer(capacity, array, arrayOffset,
          initialPosition, initialLimit, readOnly)
    }
  }

  @noinline
  private[nio] def wrap(array: Array[Byte], arrayOffset: Int, capacity: Int,
      initialPosition: Int, initialLength: Int,
      isReadOnly: Boolean): ByteBuffer = {
    GenHeapBuffer.generic_wrap(
        array, arrayOffset, capacity,
        initialPosition, initialLength, isReadOnly)
  }
}
