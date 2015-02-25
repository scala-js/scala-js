package java.nio

private[nio] final class HeapByteBufferIntView private (
    _capacity: Int,
    override private[nio] val _byteArray: Array[Byte],
    override private[nio] val _byteArrayOffset: Int,
    _initialPosition: Int, _initialLimit: Int,
    _readOnly: Boolean, override private[nio] val isBigEndian: Boolean)
    extends IntBuffer(_capacity, null, -1) {

  position(_initialPosition)
  limit(_initialLimit)

  private[this] implicit def newHeapIntBufferView =
    HeapByteBufferIntView.NewHeapByteBufferIntView

  def isReadOnly(): Boolean = _readOnly

  def isDirect(): Boolean = false

  @noinline
  def slice(): IntBuffer =
    GenHeapBufferView(this).generic_slice()

  @noinline
  def duplicate(): IntBuffer =
    GenHeapBufferView(this).generic_duplicate()

  @noinline
  def asReadOnlyBuffer(): IntBuffer =
    GenHeapBufferView(this).generic_asReadOnlyBuffer()

  @noinline
  def get(): Int =
    GenBuffer(this).generic_get()

  @noinline
  def put(c: Int): IntBuffer =
    GenBuffer(this).generic_put(c)

  @noinline
  def get(index: Int): Int =
    GenBuffer(this).generic_get(index)

  @noinline
  def put(index: Int, c: Int): IntBuffer =
    GenBuffer(this).generic_put(index, c)

  @noinline
  override def get(dst: Array[Int], offset: Int, length: Int): IntBuffer =
    GenBuffer(this).generic_get(dst, offset, length)

  @noinline
  override def put(src: Array[Int], offset: Int, length: Int): IntBuffer =
    GenBuffer(this).generic_put(src, offset, length)

  @noinline
  def compact(): IntBuffer =
    GenHeapBufferView(this).generic_compact()

  @noinline
  def order(): ByteOrder =
    GenHeapBufferView(this).generic_order()

  // Private API

  @inline
  private[nio] def load(index: Int): Int =
    GenHeapBufferView(this).byteArrayBits.loadInt(index)

  @inline
  private[nio] def store(index: Int, elem: Int): Unit =
    GenHeapBufferView(this).byteArrayBits.storeInt(index, elem)
}

private[nio] object HeapByteBufferIntView {
  private[nio] implicit object NewHeapByteBufferIntView
      extends GenHeapBufferView.NewHeapBufferView[IntBuffer] {
    def bytesPerElem: Int = 4

    def apply(capacity: Int, byteArray: Array[Byte], byteArrayOffset: Int,
        initialPosition: Int, initialLimit: Int, readOnly: Boolean,
        isBigEndian: Boolean): IntBuffer = {
      new HeapByteBufferIntView(capacity, byteArray, byteArrayOffset,
          initialPosition, initialLimit, readOnly, isBigEndian)
    }
  }

  @inline
  private[nio] def fromHeapByteBuffer(byteBuffer: HeapByteBuffer): IntBuffer =
    GenHeapBufferView.generic_fromHeapByteBuffer(byteBuffer)
}
