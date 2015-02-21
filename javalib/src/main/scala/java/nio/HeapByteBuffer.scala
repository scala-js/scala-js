package java.nio

private[nio] final class HeapByteBuffer private (
    _capacity: Int, _array0: Array[Byte], _arrayOffset0: Int,
    _initialPosition: Int, _initialLimit: Int, _readOnly: Boolean)
    extends ByteBuffer(_capacity, _array0, _arrayOffset0) {

  position(_initialPosition)
  limit(_initialLimit)

  private[this] implicit def newHeapByteBuffer = HeapByteBuffer.NewHeapByteBuffer

  def isReadOnly(): Boolean = _readOnly

  def isDirect(): Boolean = false

  @noinline
  def slice(): ByteBuffer =
    GenHeapBuffer(this).generic_slice()

  @noinline
  def duplicate(): ByteBuffer =
    GenHeapBuffer(this).generic_duplicate()

  @noinline
  def asReadOnlyBuffer(): ByteBuffer =
    GenHeapBuffer(this).generic_asReadOnlyBuffer()

  @noinline
  def get(): Byte =
    GenBuffer(this).generic_get()

  @noinline
  def put(b: Byte): ByteBuffer =
    GenBuffer(this).generic_put(b)

  @noinline
  def get(index: Int): Byte =
    GenBuffer(this).generic_get(index)

  @noinline
  def put(index: Int, b: Byte): ByteBuffer =
    GenBuffer(this).generic_put(index, b)

  @noinline
  override def get(dst: Array[Byte], offset: Int, length: Int): ByteBuffer =
    GenBuffer(this).generic_get(dst, offset, length)

  @noinline
  override def put(src: Array[Byte], offset: Int, length: Int): ByteBuffer =
    GenBuffer(this).generic_put(src, offset, length)

  @noinline
  def compact(): ByteBuffer =
    GenHeapBuffer(this).generic_compact()

  // Here begins the stuff specific to ByteArrays

  @inline private def arrayBits: ByteArrayBits =
    ByteArrayBits(_array, _arrayOffset, isBigEndian)

  @noinline def getChar(): Char =
    arrayBits.loadChar(getPosAndAdvanceRead(2))
  @noinline def putChar(value: Char): ByteBuffer =
    { ensureNotReadOnly(); arrayBits.storeChar(getPosAndAdvanceWrite(2), value); this }
  @noinline def getChar(index: Int): Char =
    arrayBits.loadChar(validateIndex(index, 2))
  @noinline def putChar(index: Int, value: Char): ByteBuffer =
    { ensureNotReadOnly(); arrayBits.storeChar(validateIndex(index, 2), value); this }

  def asCharBuffer(): CharBuffer =
    HeapByteBufferCharView.fromHeapByteBuffer(this)

  @noinline def getShort(): Short =
    arrayBits.loadShort(getPosAndAdvanceRead(2))
  @noinline def putShort(value: Short): ByteBuffer =
    { ensureNotReadOnly(); arrayBits.storeShort(getPosAndAdvanceWrite(2), value); this }
  @noinline def getShort(index: Int): Short =
    arrayBits.loadShort(validateIndex(index, 2))
  @noinline def putShort(index: Int, value: Short): ByteBuffer =
    { ensureNotReadOnly(); arrayBits.storeShort(validateIndex(index, 2), value); this }

  def asShortBuffer(): ShortBuffer =
    HeapByteBufferShortView.fromHeapByteBuffer(this)

  @noinline def getInt(): Int =
    arrayBits.loadInt(getPosAndAdvanceRead(4))
  @noinline def putInt(value: Int): ByteBuffer =
    { ensureNotReadOnly(); arrayBits.storeInt(getPosAndAdvanceWrite(4), value); this }
  @noinline def getInt(index: Int): Int =
    arrayBits.loadInt(validateIndex(index, 4))
  @noinline def putInt(index: Int, value: Int): ByteBuffer =
    { ensureNotReadOnly(); arrayBits.storeInt(validateIndex(index, 4), value); this }

  def asIntBuffer(): IntBuffer =
    HeapByteBufferIntView.fromHeapByteBuffer(this)

  @noinline def getLong(): Long =
    arrayBits.loadLong(getPosAndAdvanceRead(8))
  @noinline def putLong(value: Long): ByteBuffer =
    { ensureNotReadOnly(); arrayBits.storeLong(getPosAndAdvanceWrite(8), value); this }
  @noinline def getLong(index: Int): Long =
    arrayBits.loadLong(validateIndex(index, 8))
  @noinline def putLong(index: Int, value: Long): ByteBuffer =
    { ensureNotReadOnly(); arrayBits.storeLong(validateIndex(index, 8), value); this }

  def asLongBuffer(): LongBuffer =
    HeapByteBufferLongView.fromHeapByteBuffer(this)

  @noinline def getFloat(): Float =
    arrayBits.loadFloat(getPosAndAdvanceRead(4))
  @noinline def putFloat(value: Float): ByteBuffer =
    { ensureNotReadOnly(); arrayBits.storeFloat(getPosAndAdvanceWrite(4), value); this }
  @noinline def getFloat(index: Int): Float =
    arrayBits.loadFloat(validateIndex(index, 4))
  @noinline def putFloat(index: Int, value: Float): ByteBuffer =
    { ensureNotReadOnly(); arrayBits.storeFloat(validateIndex(index, 4), value); this }

  def asFloatBuffer(): FloatBuffer =
    HeapByteBufferFloatView.fromHeapByteBuffer(this)

  @noinline def getDouble(): Double =
    arrayBits.loadDouble(getPosAndAdvanceRead(8))
  @noinline def putDouble(value: Double): ByteBuffer =
    { ensureNotReadOnly(); arrayBits.storeDouble(getPosAndAdvanceWrite(8), value); this }
  @noinline def getDouble(index: Int): Double =
    arrayBits.loadDouble(validateIndex(index, 8))
  @noinline def putDouble(index: Int, value: Double): ByteBuffer =
    { ensureNotReadOnly(); arrayBits.storeDouble(validateIndex(index, 8), value); this }

  def asDoubleBuffer(): DoubleBuffer =
    HeapByteBufferDoubleView.fromHeapByteBuffer(this)

  // Internal API

  @inline
  private[nio] def load(index: Int): Byte =
    GenHeapBuffer(this).generic_load(index)

  @inline
  private[nio] def store(index: Int, elem: Byte): Unit =
    GenHeapBuffer(this).generic_store(index, elem)

  @inline
  override private[nio] def load(startIndex: Int,
      dst: Array[Byte], offset: Int, length: Int): Unit =
    GenHeapBuffer(this).generic_load(startIndex, dst, offset, length)

  @inline
  override private[nio] def store(startIndex: Int,
      src: Array[Byte], offset: Int, length: Int): Unit =
    GenHeapBuffer(this).generic_store(startIndex, src, offset, length)
}

private[nio] object HeapByteBuffer {
  private[nio] implicit object NewHeapByteBuffer
      extends GenHeapBuffer.NewHeapBuffer[ByteBuffer, Byte] {
    def apply(capacity: Int, array: Array[Byte], arrayOffset: Int,
        initialPosition: Int, initialLimit: Int,
        readOnly: Boolean): ByteBuffer = {
      new HeapByteBuffer(capacity, array, arrayOffset,
          initialPosition, initialLimit, readOnly)
    }
  }

  @noinline
  private[nio] def wrap(array: Array[Byte], arrayOffset: Int, capacity: Int,
      initialPosition: Int, initialLength: Int,
      isReadOnly: Boolean): ByteBuffer = {
    GenHeapBuffer.generic_wrap(
        array, arrayOffset, capacity,
        initialPosition, initialLength, isReadOnly)
  }
}
