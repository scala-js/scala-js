package java.nio

private[nio] final class HeapByteBufferLongView private (
    _capacity: Int,
    override private[nio] val _byteArray: Array[Byte],
    override private[nio] val _byteArrayOffset: Int,
    _initialPosition: Int, _initialLimit: Int,
    _readOnly: Boolean, override private[nio] val isBigEndian: Boolean)
    extends LongBuffer(_capacity, null, -1) {

  position(_initialPosition)
  limit(_initialLimit)

  private[this] implicit def newHeapLongBufferView =
    HeapByteBufferLongView.NewHeapByteBufferLongView

  def isReadOnly(): Boolean = _readOnly

  def isDirect(): Boolean = false

  @noinline
  def slice(): LongBuffer =
    GenHeapBufferView(this).generic_slice()

  @noinline
  def duplicate(): LongBuffer =
    GenHeapBufferView(this).generic_duplicate()

  @noinline
  def asReadOnlyBuffer(): LongBuffer =
    GenHeapBufferView(this).generic_asReadOnlyBuffer()

  @noinline
  def get(): Long =
    GenBuffer(this).generic_get()

  @noinline
  def put(c: Long): LongBuffer =
    GenBuffer(this).generic_put(c)

  @noinline
  def get(index: Int): Long =
    GenBuffer(this).generic_get(index)

  @noinline
  def put(index: Int, c: Long): LongBuffer =
    GenBuffer(this).generic_put(index, c)

  @noinline
  override def get(dst: Array[Long], offset: Int, length: Int): LongBuffer =
    GenBuffer(this).generic_get(dst, offset, length)

  @noinline
  override def put(src: Array[Long], offset: Int, length: Int): LongBuffer =
    GenBuffer(this).generic_put(src, offset, length)

  @noinline
  def compact(): LongBuffer =
    GenHeapBufferView(this).generic_compact()

  @noinline
  def order(): ByteOrder =
    GenHeapBufferView(this).generic_order()

  // Private API

  @inline
  private[nio] def load(index: Int): Long =
    GenHeapBufferView(this).byteArrayBits.loadLong(index)

  @inline
  private[nio] def store(index: Int, elem: Long): Unit =
    GenHeapBufferView(this).byteArrayBits.storeLong(index, elem)
}

private[nio] object HeapByteBufferLongView {
  private[nio] implicit object NewHeapByteBufferLongView
      extends GenHeapBufferView.NewHeapBufferView[LongBuffer] {
    def bytesPerElem: Int = 8

    def apply(capacity: Int, byteArray: Array[Byte], byteArrayOffset: Int,
        initialPosition: Int, initialLimit: Int, readOnly: Boolean,
        isBigEndian: Boolean): LongBuffer = {
      new HeapByteBufferLongView(capacity, byteArray, byteArrayOffset,
          initialPosition, initialLimit, readOnly, isBigEndian)
    }
  }

  @inline
  private[nio] def fromHeapByteBuffer(byteBuffer: HeapByteBuffer): LongBuffer =
    GenHeapBufferView.generic_fromHeapByteBuffer(byteBuffer)
}
