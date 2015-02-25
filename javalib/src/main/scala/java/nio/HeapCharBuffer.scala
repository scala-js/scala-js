package java.nio

private[nio] final class HeapCharBuffer private (
    _capacity: Int, _array0: Array[Char], _arrayOffset0: Int,
    _initialPosition: Int, _initialLimit: Int, _readOnly: Boolean)
    extends CharBuffer(_capacity, _array0, _arrayOffset0) {

  position(_initialPosition)
  limit(_initialLimit)

  private[this] implicit def newHeapCharBuffer = HeapCharBuffer.NewHeapCharBuffer

  def isReadOnly(): Boolean = _readOnly

  def isDirect(): Boolean = false

  @noinline
  def slice(): CharBuffer =
    GenHeapBuffer(this).generic_slice()

  @noinline
  def duplicate(): CharBuffer =
    GenHeapBuffer(this).generic_duplicate()

  @noinline
  def asReadOnlyBuffer(): CharBuffer =
    GenHeapBuffer(this).generic_asReadOnlyBuffer()

  def subSequence(start: Int, end: Int): CharBuffer = {
    if (start < 0 || end < start || end > remaining)
      throw new IndexOutOfBoundsException
    new HeapCharBuffer(capacity, _array, _arrayOffset,
        position + start, position + end, isReadOnly)
  }

  @noinline
  def get(): Char =
    GenBuffer(this).generic_get()

  @noinline
  def put(c: Char): CharBuffer =
    GenBuffer(this).generic_put(c)

  @noinline
  def get(index: Int): Char =
    GenBuffer(this).generic_get(index)

  @noinline
  def put(index: Int, c: Char): CharBuffer =
    GenBuffer(this).generic_put(index, c)

  @noinline
  override def get(dst: Array[Char], offset: Int, length: Int): CharBuffer =
    GenBuffer(this).generic_get(dst, offset, length)

  @noinline
  override def put(src: Array[Char], offset: Int, length: Int): CharBuffer =
    GenBuffer(this).generic_put(src, offset, length)

  @noinline
  def compact(): CharBuffer =
    GenHeapBuffer(this).generic_compact()

  def order(): ByteOrder = ByteOrder.nativeOrder()

  // Internal API

  @inline
  private[nio] def load(index: Int): Char =
    GenHeapBuffer(this).generic_load(index)

  @inline
  private[nio] def store(index: Int, elem: Char): Unit =
    GenHeapBuffer(this).generic_store(index, elem)

  @inline
  override private[nio] def load(startIndex: Int,
      dst: Array[Char], offset: Int, length: Int): Unit =
    GenHeapBuffer(this).generic_load(startIndex, dst, offset, length)

  @inline
  override private[nio] def store(startIndex: Int,
      src: Array[Char], offset: Int, length: Int): Unit =
    GenHeapBuffer(this).generic_store(startIndex, src, offset, length)
}

private[nio] object HeapCharBuffer {
  private[nio] implicit object NewHeapCharBuffer
      extends GenHeapBuffer.NewHeapBuffer[CharBuffer, Char] {
    def apply(capacity: Int, array: Array[Char], arrayOffset: Int,
        initialPosition: Int, initialLimit: Int,
        readOnly: Boolean): CharBuffer = {
      new HeapCharBuffer(capacity, array, arrayOffset,
          initialPosition, initialLimit, readOnly)
    }
  }

  private[nio] def wrap(array: Array[Char], arrayOffset: Int, capacity: Int,
      initialPosition: Int, initialLength: Int,
      isReadOnly: Boolean): CharBuffer = {
    GenHeapBuffer.generic_wrap(
        array, arrayOffset, capacity,
        initialPosition, initialLength, isReadOnly)
  }
}
