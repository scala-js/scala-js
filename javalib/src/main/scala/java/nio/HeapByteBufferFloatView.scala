package java.nio

private[nio] final class HeapByteBufferFloatView private (
    _capacity: Int,
    override private[nio] val _byteArray: Array[Byte],
    override private[nio] val _byteArrayOffset: Int,
    _initialPosition: Int, _initialLimit: Int,
    _readOnly: Boolean, override private[nio] val isBigEndian: Boolean)
    extends FloatBuffer(_capacity, null, -1) {

  position(_initialPosition)
  limit(_initialLimit)

  private[this] implicit def newHeapFloatBufferView =
    HeapByteBufferFloatView.NewHeapByteBufferFloatView

  def isReadOnly(): Boolean = _readOnly

  def isDirect(): Boolean = false

  @noinline
  def slice(): FloatBuffer =
    GenHeapBufferView(this).generic_slice()

  @noinline
  def duplicate(): FloatBuffer =
    GenHeapBufferView(this).generic_duplicate()

  @noinline
  def asReadOnlyBuffer(): FloatBuffer =
    GenHeapBufferView(this).generic_asReadOnlyBuffer()

  @noinline
  def get(): Float =
    GenBuffer(this).generic_get()

  @noinline
  def put(c: Float): FloatBuffer =
    GenBuffer(this).generic_put(c)

  @noinline
  def get(index: Int): Float =
    GenBuffer(this).generic_get(index)

  @noinline
  def put(index: Int, c: Float): FloatBuffer =
    GenBuffer(this).generic_put(index, c)

  @noinline
  override def get(dst: Array[Float], offset: Int, length: Int): FloatBuffer =
    GenBuffer(this).generic_get(dst, offset, length)

  @noinline
  override def put(src: Array[Float], offset: Int, length: Int): FloatBuffer =
    GenBuffer(this).generic_put(src, offset, length)

  @noinline
  def compact(): FloatBuffer =
    GenHeapBufferView(this).generic_compact()

  @noinline
  def order(): ByteOrder =
    GenHeapBufferView(this).generic_order()

  // Private API

  @inline
  private[nio] def load(index: Int): Float =
    GenHeapBufferView(this).byteArrayBits.loadFloat(index)

  @inline
  private[nio] def store(index: Int, elem: Float): Unit =
    GenHeapBufferView(this).byteArrayBits.storeFloat(index, elem)
}

private[nio] object HeapByteBufferFloatView {
  private[nio] implicit object NewHeapByteBufferFloatView
      extends GenHeapBufferView.NewHeapBufferView[FloatBuffer] {
    def bytesPerElem: Int = 4

    def apply(capacity: Int, byteArray: Array[Byte], byteArrayOffset: Int,
        initialPosition: Int, initialLimit: Int, readOnly: Boolean,
        isBigEndian: Boolean): FloatBuffer = {
      new HeapByteBufferFloatView(capacity, byteArray, byteArrayOffset,
          initialPosition, initialLimit, readOnly, isBigEndian)
    }
  }

  @inline
  private[nio] def fromHeapByteBuffer(byteBuffer: HeapByteBuffer): FloatBuffer =
    GenHeapBufferView.generic_fromHeapByteBuffer(byteBuffer)
}
