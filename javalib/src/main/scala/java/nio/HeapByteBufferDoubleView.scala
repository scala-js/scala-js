package java.nio

private[nio] final class HeapByteBufferDoubleView private (
    _capacity: Int,
    override private[nio] val _byteArray: Array[Byte],
    override private[nio] val _byteArrayOffset: Int,
    _initialPosition: Int, _initialLimit: Int,
    _readOnly: Boolean, override private[nio] val isBigEndian: Boolean)
    extends DoubleBuffer(_capacity, null, -1) {

  position(_initialPosition)
  limit(_initialLimit)

  private[this] implicit def newHeapDoubleBufferView =
    HeapByteBufferDoubleView.NewHeapByteBufferDoubleView

  def isReadOnly(): Boolean = _readOnly

  def isDirect(): Boolean = false

  @noinline
  def slice(): DoubleBuffer =
    GenHeapBufferView(this).generic_slice()

  @noinline
  def duplicate(): DoubleBuffer =
    GenHeapBufferView(this).generic_duplicate()

  @noinline
  def asReadOnlyBuffer(): DoubleBuffer =
    GenHeapBufferView(this).generic_asReadOnlyBuffer()

  @noinline
  def get(): Double =
    GenBuffer(this).generic_get()

  @noinline
  def put(c: Double): DoubleBuffer =
    GenBuffer(this).generic_put(c)

  @noinline
  def get(index: Int): Double =
    GenBuffer(this).generic_get(index)

  @noinline
  def put(index: Int, c: Double): DoubleBuffer =
    GenBuffer(this).generic_put(index, c)

  @noinline
  override def get(dst: Array[Double], offset: Int, length: Int): DoubleBuffer =
    GenBuffer(this).generic_get(dst, offset, length)

  @noinline
  override def put(src: Array[Double], offset: Int, length: Int): DoubleBuffer =
    GenBuffer(this).generic_put(src, offset, length)

  @noinline
  def compact(): DoubleBuffer =
    GenHeapBufferView(this).generic_compact()

  @noinline
  def order(): ByteOrder =
    GenHeapBufferView(this).generic_order()

  // Private API

  @inline
  private[nio] def load(index: Int): Double =
    GenHeapBufferView(this).byteArrayBits.loadDouble(index)

  @inline
  private[nio] def store(index: Int, elem: Double): Unit =
    GenHeapBufferView(this).byteArrayBits.storeDouble(index, elem)
}

private[nio] object HeapByteBufferDoubleView {
  private[nio] implicit object NewHeapByteBufferDoubleView
      extends GenHeapBufferView.NewHeapBufferView[DoubleBuffer] {
    def bytesPerElem: Int = 8

    def apply(capacity: Int, byteArray: Array[Byte], byteArrayOffset: Int,
        initialPosition: Int, initialLimit: Int, readOnly: Boolean,
        isBigEndian: Boolean): DoubleBuffer = {
      new HeapByteBufferDoubleView(capacity, byteArray, byteArrayOffset,
          initialPosition, initialLimit, readOnly, isBigEndian)
    }
  }

  @inline
  private[nio] def fromHeapByteBuffer(byteBuffer: HeapByteBuffer): DoubleBuffer =
    GenHeapBufferView.generic_fromHeapByteBuffer(byteBuffer)
}
