package java.nio

final class ByteOrder private (name: String) {
  override def toString(): String = name
}

object ByteOrder {
  val BIG_ENDIAN: ByteOrder = new ByteOrder("BIG_ENDIAN")
  val LITTLE_ENDIAN: ByteOrder = new ByteOrder("LITTLE_ENDIAN")

  def nativeOrder(): ByteOrder = {
    if (scala.scalajs.runtime.Bits.areTypedArraysBigEndian) BIG_ENDIAN
    else LITTLE_ENDIAN
  }
}
