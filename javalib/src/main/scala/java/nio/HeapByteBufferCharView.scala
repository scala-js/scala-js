package java.nio

private[nio] final class HeapByteBufferCharView private (
    _capacity: Int,
    override private[nio] val _byteArray: Array[Byte],
    override private[nio] val _byteArrayOffset: Int,
    _initialPosition: Int, _initialLimit: Int,
    _readOnly: Boolean, override private[nio] val isBigEndian: Boolean)
    extends CharBuffer(_capacity, null, -1) {

  position(_initialPosition)
  limit(_initialLimit)

  private[this] implicit def newHeapCharBufferView =
    HeapByteBufferCharView.NewHeapByteBufferCharView

  def isReadOnly(): Boolean = _readOnly

  def isDirect(): Boolean = false

  @noinline
  def slice(): CharBuffer =
    GenHeapBufferView(this).generic_slice()

  @noinline
  def duplicate(): CharBuffer =
    GenHeapBufferView(this).generic_duplicate()

  @noinline
  def asReadOnlyBuffer(): CharBuffer =
    GenHeapBufferView(this).generic_asReadOnlyBuffer()

  def subSequence(start: Int, end: Int): CharBuffer = {
    if (start < 0 || end < start || end > remaining)
      throw new IndexOutOfBoundsException
    new HeapByteBufferCharView(capacity, _byteArray, _byteArrayOffset,
        position + start, position + end, isReadOnly, isBigEndian)
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
    GenHeapBufferView(this).generic_compact()

  @noinline
  def order(): ByteOrder =
    GenHeapBufferView(this).generic_order()

  // Private API

  @inline
  private[nio] def load(index: Int): Char =
    GenHeapBufferView(this).byteArrayBits.loadChar(index)

  @inline
  private[nio] def store(index: Int, elem: Char): Unit =
    GenHeapBufferView(this).byteArrayBits.storeChar(index, elem)
}

private[nio] object HeapByteBufferCharView {
  private[nio] implicit object NewHeapByteBufferCharView
      extends GenHeapBufferView.NewHeapBufferView[CharBuffer] {
    def bytesPerElem: Int = 2

    def apply(capacity: Int, byteArray: Array[Byte], byteArrayOffset: Int,
        initialPosition: Int, initialLimit: Int, readOnly: Boolean,
        isBigEndian: Boolean): CharBuffer = {
      new HeapByteBufferCharView(capacity, byteArray, byteArrayOffset,
          initialPosition, initialLimit, readOnly, isBigEndian)
    }
  }

  @inline
  private[nio] def fromHeapByteBuffer(byteBuffer: HeapByteBuffer): CharBuffer =
    GenHeapBufferView.generic_fromHeapByteBuffer(byteBuffer)
}
