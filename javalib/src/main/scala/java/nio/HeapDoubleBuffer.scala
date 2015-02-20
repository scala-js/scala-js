package java.nio

private[nio] final class HeapDoubleBuffer private (
    _capacity: Int, _array0: Array[Double], _arrayOffset0: Int,
    _initialPosition: Int, _initialLimit: Int, _readOnly: Boolean)
    extends DoubleBuffer(_capacity, _array0, _arrayOffset0) {

  position(_initialPosition)
  limit(_initialLimit)

  private[this] implicit def newHeapDoubleBuffer = HeapDoubleBuffer.NewHeapDoubleBuffer

  def isReadOnly(): Boolean = _readOnly

  def isDirect(): Boolean = false

  @noinline
  def slice(): DoubleBuffer =
    GenHeapBuffer(this).generic_slice()

  @noinline
  def duplicate(): DoubleBuffer =
    GenHeapBuffer(this).generic_duplicate()

  @noinline
  def asReadOnlyBuffer(): DoubleBuffer =
    GenHeapBuffer(this).generic_asReadOnlyBuffer()

  @noinline
  def get(): Double =
    GenBuffer(this).generic_get()

  @noinline
  def put(d: Double): DoubleBuffer =
    GenBuffer(this).generic_put(d)

  @noinline
  def get(index: Int): Double =
    GenBuffer(this).generic_get(index)

  @noinline
  def put(index: Int, d: Double): DoubleBuffer =
    GenBuffer(this).generic_put(index, d)

  @noinline
  override def get(dst: Array[Double], offset: Int, length: Int): DoubleBuffer =
    GenBuffer(this).generic_get(dst, offset, length)

  @noinline
  override def put(src: Array[Double], offset: Int, length: Int): DoubleBuffer =
    GenBuffer(this).generic_put(src, offset, length)

  @noinline
  def compact(): DoubleBuffer =
    GenHeapBuffer(this).generic_compact()

  def order(): ByteOrder = ByteOrder.nativeOrder()

  // Internal API

  @inline
  private[nio] def load(index: Int): Double =
    GenHeapBuffer(this).generic_load(index)

  @inline
  private[nio] def store(index: Int, elem: Double): Unit =
    GenHeapBuffer(this).generic_store(index, elem)

  @inline
  override private[nio] def load(startIndex: Int,
      dst: Array[Double], offset: Int, length: Int): Unit =
    GenHeapBuffer(this).generic_load(startIndex, dst, offset, length)

  @inline
  override private[nio] def store(startIndex: Int,
      src: Array[Double], offset: Int, length: Int): Unit =
    GenHeapBuffer(this).generic_store(startIndex, src, offset, length)
}

private[nio] object HeapDoubleBuffer {
  private[nio] implicit object NewHeapDoubleBuffer
      extends GenHeapBuffer.NewHeapBuffer[DoubleBuffer, Double] {
    def apply(capacity: Int, array: Array[Double], arrayOffset: Int,
        initialPosition: Int, initialLimit: Int,
        readOnly: Boolean): DoubleBuffer = {
      new HeapDoubleBuffer(capacity, array, arrayOffset,
          initialPosition, initialLimit, readOnly)
    }
  }

  @noinline
  private[nio] def wrap(array: Array[Double], arrayOffset: Int, capacity: Int,
      initialPosition: Int, initialLength: Int,
      isReadOnly: Boolean): DoubleBuffer = {
    GenHeapBuffer.generic_wrap(
        array, arrayOffset, capacity,
        initialPosition, initialLength, isReadOnly)
  }
}
