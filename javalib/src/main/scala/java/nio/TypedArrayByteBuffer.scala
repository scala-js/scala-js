package java.nio

import scala.scalajs.js.typedarray._
import DataViewExt._

private[nio] final class TypedArrayByteBuffer private (
    override private[nio] val _typedArray: Int8Array,
    _initialPosition: Int, _initialLimit: Int, _readOnly: Boolean)
    extends ByteBuffer(_typedArray.length, null, -1) {

  override private[nio] lazy val _dataView: DataView =
    new DataView(_typedArray.buffer, _typedArray.byteOffset, capacity)

  position(_initialPosition)
  limit(_initialLimit)

  private[this] implicit def newTypedArrayByteBuffer =
    TypedArrayByteBuffer.NewTypedArrayByteBuffer

  def isReadOnly(): Boolean = _readOnly

  def isDirect(): Boolean = true

  @noinline
  def slice(): ByteBuffer =
    GenTypedArrayBuffer(this).generic_slice()

  @noinline
  def duplicate(): ByteBuffer =
    GenTypedArrayBuffer(this).generic_duplicate()

  @noinline
  def asReadOnlyBuffer(): ByteBuffer =
    GenTypedArrayBuffer(this).generic_asReadOnlyBuffer()

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
    GenTypedArrayBuffer(this).generic_compact()

  // Here begins the stuff specific to ByteArrays

  @inline
  def hasNativeOrder: Boolean =
    isBigEndian == scala.scalajs.runtime.Bits.areTypedArraysBigEndian

  @noinline def getChar(): Char =
    _dataView.getUint16(getPosAndAdvanceRead(2), !isBigEndian).toChar
  @noinline def putChar(value: Char): ByteBuffer =
    { ensureNotReadOnly(); _dataView.setUint16(getPosAndAdvanceWrite(2), value, !isBigEndian); this }
  @noinline def getChar(index: Int): Char =
    _dataView.getUint16(validateIndex(index, 2), !isBigEndian).toChar
  @noinline def putChar(index: Int, value: Char): ByteBuffer =
    { ensureNotReadOnly(); _dataView.setUint16(validateIndex(index, 2), value, !isBigEndian); this }

  def asCharBuffer(): CharBuffer = {
    if (hasNativeOrder && (_arrayBufferOffset + position) % 2 == 0)
      TypedArrayCharBuffer.fromTypedArrayByteBuffer(this)
    else
      DataViewCharBuffer.fromTypedArrayByteBuffer(this)
  }

  @noinline def getShort(): Short =
    _dataView.getInt16(getPosAndAdvanceRead(2), !isBigEndian)
  @noinline def putShort(value: Short): ByteBuffer =
    { ensureNotReadOnly(); _dataView.setInt16(getPosAndAdvanceWrite(2), value, !isBigEndian); this }
  @noinline def getShort(index: Int): Short =
    _dataView.getInt16(validateIndex(index, 2), !isBigEndian)
  @noinline def putShort(index: Int, value: Short): ByteBuffer =
    { ensureNotReadOnly(); _dataView.setInt16(validateIndex(index, 2), value, !isBigEndian); this }

  def asShortBuffer(): ShortBuffer = {
    if (hasNativeOrder && (_arrayBufferOffset + position) % 2 == 0)
      TypedArrayShortBuffer.fromTypedArrayByteBuffer(this)
    else
      DataViewShortBuffer.fromTypedArrayByteBuffer(this)
  }

  @noinline def getInt(): Int =
    _dataView.getInt32(getPosAndAdvanceRead(4), !isBigEndian)
  @noinline def putInt(value: Int): ByteBuffer =
    { ensureNotReadOnly(); _dataView.setInt32(getPosAndAdvanceWrite(4), value, !isBigEndian); this }
  @noinline def getInt(index: Int): Int =
    _dataView.getInt32(validateIndex(index, 4), !isBigEndian)
  @noinline def putInt(index: Int, value: Int): ByteBuffer =
    { ensureNotReadOnly(); _dataView.setInt32(validateIndex(index, 4), value, !isBigEndian); this }

  def asIntBuffer(): IntBuffer = {
    if (hasNativeOrder && (_arrayBufferOffset + position) % 4 == 0)
      TypedArrayIntBuffer.fromTypedArrayByteBuffer(this)
    else
      DataViewIntBuffer.fromTypedArrayByteBuffer(this)
  }

  @noinline def getLong(): Long =
    _dataView.getInt64(getPosAndAdvanceRead(8), !isBigEndian)
  @noinline def putLong(value: Long): ByteBuffer =
    { ensureNotReadOnly(); _dataView.setInt64(getPosAndAdvanceWrite(8), value, !isBigEndian); this }
  @noinline def getLong(index: Int): Long =
    _dataView.getInt64(validateIndex(index, 8), !isBigEndian)
  @noinline def putLong(index: Int, value: Long): ByteBuffer =
    { ensureNotReadOnly(); _dataView.setInt64(validateIndex(index, 8), value, !isBigEndian); this }

  def asLongBuffer(): LongBuffer =
    DataViewLongBuffer.fromTypedArrayByteBuffer(this)

  @noinline def getFloat(): Float =
    _dataView.getFloat32(getPosAndAdvanceRead(4), !isBigEndian)
  @noinline def putFloat(value: Float): ByteBuffer =
    { ensureNotReadOnly(); _dataView.setFloat32(getPosAndAdvanceWrite(4), value, !isBigEndian); this }
  @noinline def getFloat(index: Int): Float =
    _dataView.getFloat32(validateIndex(index, 4), !isBigEndian)
  @noinline def putFloat(index: Int, value: Float): ByteBuffer =
    { ensureNotReadOnly(); _dataView.setFloat32(validateIndex(index, 4), value, !isBigEndian); this }

  def asFloatBuffer(): FloatBuffer = {
    if (hasNativeOrder && (_arrayBufferOffset + position) % 4 == 0)
      TypedArrayFloatBuffer.fromTypedArrayByteBuffer(this)
    else
      DataViewFloatBuffer.fromTypedArrayByteBuffer(this)
  }

  @noinline def getDouble(): Double =
    _dataView.getFloat64(getPosAndAdvanceRead(8), !isBigEndian)
  @noinline def putDouble(value: Double): ByteBuffer =
    { ensureNotReadOnly(); _dataView.setFloat64(getPosAndAdvanceWrite(8), value, !isBigEndian); this }
  @noinline def getDouble(index: Int): Double =
    _dataView.getFloat64(validateIndex(index, 8), !isBigEndian)
  @noinline def putDouble(index: Int, value: Double): ByteBuffer =
    { ensureNotReadOnly(); _dataView.setFloat64(validateIndex(index, 8), value, !isBigEndian); this }

  def asDoubleBuffer(): DoubleBuffer = {
    if (hasNativeOrder && (_arrayBufferOffset + position) % 8 == 0)
      TypedArrayDoubleBuffer.fromTypedArrayByteBuffer(this)
    else
      DataViewDoubleBuffer.fromTypedArrayByteBuffer(this)
  }

  // Internal API

  @inline
  override private[nio] def _arrayBuffer: ArrayBuffer =
    GenTypedArrayBuffer(this).generic_arrayBuffer

  @inline
  override private[nio] def _arrayBufferOffset: Int =
    GenTypedArrayBuffer(this).generic_arrayBufferOffset

  @inline
  private[nio] def load(index: Int): Byte =
    _typedArray(index)

  @inline
  private[nio] def store(index: Int, elem: Byte): Unit =
    _typedArray(index) = elem

  @inline
  override private[nio] def load(startIndex: Int,
      dst: Array[Byte], offset: Int, length: Int): Unit =
    GenBuffer(this).generic_load(startIndex, dst, offset, length)

  @inline
  override private[nio] def store(startIndex: Int,
      src: Array[Byte], offset: Int, length: Int): Unit =
    GenBuffer(this).generic_store(startIndex, src, offset, length)
}

private[nio] object TypedArrayByteBuffer {
  private[nio] implicit object NewTypedArrayByteBuffer
      extends GenTypedArrayBuffer.NewTypedArrayBuffer[ByteBuffer] {
    def bytesPerElem: Int = 1

    def apply(typedArray: Int8Array,
        initialPosition: Int, initialLimit: Int,
        readOnly: Boolean): TypedArrayByteBuffer = {
      new TypedArrayByteBuffer(typedArray,
          initialPosition, initialLimit, readOnly)
    }

    @inline
    def newTypedArray(buffer: ArrayBuffer,
        byteOffset: Int, length: Int): Int8Array = {
      new Int8Array(buffer, byteOffset, length)
    }
  }

  def allocate(capacity: Int): ByteBuffer = {
    if (capacity < 0)
      throw new IllegalArgumentException
    new TypedArrayByteBuffer(new Int8Array(capacity), 0, capacity, false)
  }

  def wrap(array: ArrayBuffer): ByteBuffer =
    wrap(new Int8Array(array))

  def wrap(array: ArrayBuffer, byteOffset: Int, length: Int): ByteBuffer =
    wrap(new Int8Array(array, byteOffset, length))

  def wrap(typedArray: Int8Array): ByteBuffer = {
    val buf = new TypedArrayByteBuffer(typedArray, 0, typedArray.length, false)
    buf._isBigEndian = scala.scalajs.runtime.Bits.areTypedArraysBigEndian
    buf
  }
}
