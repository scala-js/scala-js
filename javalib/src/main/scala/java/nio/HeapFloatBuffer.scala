package java.nio

private[nio] final class HeapFloatBuffer private (
    _capacity: Int, _array0: Array[Float], _arrayOffset0: Int,
    _initialPosition: Int, _initialLimit: Int, _readOnly: Boolean)
    extends FloatBuffer(_capacity, _array0, _arrayOffset0) {

  position(_initialPosition)
  limit(_initialLimit)

  private[this] implicit def newHeapFloatBuffer = HeapFloatBuffer.NewHeapFloatBuffer

  def isReadOnly(): Boolean = _readOnly

  def isDirect(): Boolean = false

  @noinline
  def slice(): FloatBuffer =
    GenHeapBuffer(this).generic_slice()

  @noinline
  def duplicate(): FloatBuffer =
    GenHeapBuffer(this).generic_duplicate()

  @noinline
  def asReadOnlyBuffer(): FloatBuffer =
    GenHeapBuffer(this).generic_asReadOnlyBuffer()

  @noinline
  def get(): Float =
    GenBuffer(this).generic_get()

  @noinline
  def put(f: Float): FloatBuffer =
    GenBuffer(this).generic_put(f)

  @noinline
  def get(index: Int): Float =
    GenBuffer(this).generic_get(index)

  @noinline
  def put(index: Int, f: Float): FloatBuffer =
    GenBuffer(this).generic_put(index, f)

  @noinline
  override def get(dst: Array[Float], offset: Int, length: Int): FloatBuffer =
    GenBuffer(this).generic_get(dst, offset, length)

  @noinline
  override def put(src: Array[Float], offset: Int, length: Int): FloatBuffer =
    GenBuffer(this).generic_put(src, offset, length)

  @noinline
  def compact(): FloatBuffer =
    GenHeapBuffer(this).generic_compact()

  def order(): ByteOrder = ByteOrder.nativeOrder()

  // Internal API

  @inline
  private[nio] def load(index: Int): Float =
    GenHeapBuffer(this).generic_load(index)

  @inline
  private[nio] def store(index: Int, elem: Float): Unit =
    GenHeapBuffer(this).generic_store(index, elem)

  @inline
  override private[nio] def load(startIndex: Int,
      dst: Array[Float], offset: Int, length: Int): Unit =
    GenHeapBuffer(this).generic_load(startIndex, dst, offset, length)

  @inline
  override private[nio] def store(startIndex: Int,
      src: Array[Float], offset: Int, length: Int): Unit =
    GenHeapBuffer(this).generic_store(startIndex, src, offset, length)
}

private[nio] object HeapFloatBuffer {
  private[nio] implicit object NewHeapFloatBuffer
      extends GenHeapBuffer.NewHeapBuffer[FloatBuffer, Float] {
    def apply(capacity: Int, array: Array[Float], arrayOffset: Int,
        initialPosition: Int, initialLimit: Int,
        readOnly: Boolean): FloatBuffer = {
      new HeapFloatBuffer(capacity, array, arrayOffset,
          initialPosition, initialLimit, readOnly)
    }
  }

  @noinline
  private[nio] def wrap(array: Array[Float], arrayOffset: Int, capacity: Int,
      initialPosition: Int, initialLength: Int,
      isReadOnly: Boolean): FloatBuffer = {
    GenHeapBuffer.generic_wrap(
        array, arrayOffset, capacity,
        initialPosition, initialLength, isReadOnly)
  }
}
