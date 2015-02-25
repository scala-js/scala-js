package java.nio

private[nio] final class HeapIntBuffer private (
    _capacity: Int, _array0: Array[Int], _arrayOffset0: Int,
    _initialPosition: Int, _initialLimit: Int, _readOnly: Boolean)
    extends IntBuffer(_capacity, _array0, _arrayOffset0) {

  position(_initialPosition)
  limit(_initialLimit)

  private[this] implicit def newHeapIntBuffer = HeapIntBuffer.NewHeapIntBuffer

  def isReadOnly(): Boolean = _readOnly

  def isDirect(): Boolean = false

  @noinline
  def slice(): IntBuffer =
    GenHeapBuffer(this).generic_slice()

  @noinline
  def duplicate(): IntBuffer =
    GenHeapBuffer(this).generic_duplicate()

  @noinline
  def asReadOnlyBuffer(): IntBuffer =
    GenHeapBuffer(this).generic_asReadOnlyBuffer()

  @noinline
  def get(): Int =
    GenBuffer(this).generic_get()

  @noinline
  def put(i: Int): IntBuffer =
    GenBuffer(this).generic_put(i)

  @noinline
  def get(index: Int): Int =
    GenBuffer(this).generic_get(index)

  @noinline
  def put(index: Int, i: Int): IntBuffer =
    GenBuffer(this).generic_put(index, i)

  @noinline
  override def get(dst: Array[Int], offset: Int, length: Int): IntBuffer =
    GenBuffer(this).generic_get(dst, offset, length)

  @noinline
  override def put(src: Array[Int], offset: Int, length: Int): IntBuffer =
    GenBuffer(this).generic_put(src, offset, length)

  @noinline
  def compact(): IntBuffer =
    GenHeapBuffer(this).generic_compact()

  def order(): ByteOrder = ByteOrder.nativeOrder()

  // Internal API

  @inline
  private[nio] def load(index: Int): Int =
    GenHeapBuffer(this).generic_load(index)

  @inline
  private[nio] def store(index: Int, elem: Int): Unit =
    GenHeapBuffer(this).generic_store(index, elem)

  @inline
  override private[nio] def load(startIndex: Int,
      dst: Array[Int], offset: Int, length: Int): Unit =
    GenHeapBuffer(this).generic_load(startIndex, dst, offset, length)

  @inline
  override private[nio] def store(startIndex: Int,
      src: Array[Int], offset: Int, length: Int): Unit =
    GenHeapBuffer(this).generic_store(startIndex, src, offset, length)
}

private[nio] object HeapIntBuffer {
  private[nio] implicit object NewHeapIntBuffer
      extends GenHeapBuffer.NewHeapBuffer[IntBuffer, Int] {
    def apply(capacity: Int, array: Array[Int], arrayOffset: Int,
        initialPosition: Int, initialLimit: Int,
        readOnly: Boolean): IntBuffer = {
      new HeapIntBuffer(capacity, array, arrayOffset,
          initialPosition, initialLimit, readOnly)
    }
  }

  @noinline
  private[nio] def wrap(array: Array[Int], arrayOffset: Int, capacity: Int,
      initialPosition: Int, initialLength: Int,
      isReadOnly: Boolean): IntBuffer = {
    GenHeapBuffer.generic_wrap(
        array, arrayOffset, capacity,
        initialPosition, initialLength, isReadOnly)
  }
}
