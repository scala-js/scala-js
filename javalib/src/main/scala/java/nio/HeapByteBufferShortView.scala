package java.nio

private[nio] final class HeapByteBufferShortView private (
    _capacity: Int,
    override private[nio] val _byteArray: Array[Byte],
    override private[nio] val _byteArrayOffset: Int,
    _initialPosition: Int, _initialLimit: Int,
    _readOnly: Boolean, override private[nio] val isBigEndian: Boolean)
    extends ShortBuffer(_capacity, null, -1) {

  position(_initialPosition)
  limit(_initialLimit)

  private[this] implicit def newHeapShortBufferView =
    HeapByteBufferShortView.NewHeapByteBufferShortView

  def isReadOnly(): Boolean = _readOnly

  def isDirect(): Boolean = false

  @noinline
  def slice(): ShortBuffer =
    GenHeapBufferView(this).generic_slice()

  @noinline
  def duplicate(): ShortBuffer =
    GenHeapBufferView(this).generic_duplicate()

  @noinline
  def asReadOnlyBuffer(): ShortBuffer =
    GenHeapBufferView(this).generic_asReadOnlyBuffer()

  @noinline
  def get(): Short =
    GenBuffer(this).generic_get()

  @noinline
  def put(c: Short): ShortBuffer =
    GenBuffer(this).generic_put(c)

  @noinline
  def get(index: Int): Short =
    GenBuffer(this).generic_get(index)

  @noinline
  def put(index: Int, c: Short): ShortBuffer =
    GenBuffer(this).generic_put(index, c)

  @noinline
  override def get(dst: Array[Short], offset: Int, length: Int): ShortBuffer =
    GenBuffer(this).generic_get(dst, offset, length)

  @noinline
  override def put(src: Array[Short], offset: Int, length: Int): ShortBuffer =
    GenBuffer(this).generic_put(src, offset, length)

  @noinline
  def compact(): ShortBuffer =
    GenHeapBufferView(this).generic_compact()

  @noinline
  def order(): ByteOrder =
    GenHeapBufferView(this).generic_order()

  // Private API

  @inline
  private[nio] def load(index: Int): Short =
    GenHeapBufferView(this).byteArrayBits.loadShort(index)

  @inline
  private[nio] def store(index: Int, elem: Short): Unit =
    GenHeapBufferView(this).byteArrayBits.storeShort(index, elem)
}

private[nio] object HeapByteBufferShortView {
  private[nio] implicit object NewHeapByteBufferShortView
      extends GenHeapBufferView.NewHeapBufferView[ShortBuffer] {
    def bytesPerElem: Int = 2

    def apply(capacity: Int, byteArray: Array[Byte], byteArrayOffset: Int,
        initialPosition: Int, initialLimit: Int, readOnly: Boolean,
        isBigEndian: Boolean): ShortBuffer = {
      new HeapByteBufferShortView(capacity, byteArray, byteArrayOffset,
          initialPosition, initialLimit, readOnly, isBigEndian)
    }
  }

  @inline
  private[nio] def fromHeapByteBuffer(byteBuffer: HeapByteBuffer): ShortBuffer =
    GenHeapBufferView.generic_fromHeapByteBuffer(byteBuffer)
}
