package java.util.zip

// scalastyle:off equals.hash.code

/** Pure Scala implementation of ZipEntry */
class ZipEntry(private[this] val _name: String) extends Cloneable {

  private[this] var _comment: String = null
  private[this] var _csize: Long = -1
  private[this] var _crc: Long = -1
  private[this] var _extra: Array[Byte] = null
  private[this] var _method: Int = -1
  private[this] var _size: Long = -1
  private[this] var _time: Long = -1

  def this(e: ZipEntry) = {
    this(e.getName())
    setComment(e.getComment())
    setCompressedSize(e.getCompressedSize())
    setCrc(e.getCrc())
    setExtra(e.getExtra())
    setMethod(e.getMethod())
    setSize(e.getSize())
    setTime(e.getTime())
  }

  override def clone(): Object = {
    val result = super.clone()
    if (getExtra() != null)
      setExtra(getExtra().clone().asInstanceOf[Array[Byte]])
    result
  }

  def getComment(): String = _comment
  def getCompressedSize(): Long = _csize
  def getCrc(): Long = _crc
  def getExtra(): Array[Byte] = _extra
  def getMethod(): Int = _method
  def getName(): String = _name
  def getSize(): Long = _size
  def getTime(): Long = _time

  // Strangely, the Javalib defines hashCode, but not equals.
  override def hashCode(): Int = {
    import scala.util.hashing.MurmurHash3._

    var acc = 0x45322
    acc = mix(acc, getComment.##)
    acc = mix(acc, getCompressedSize.##)
    acc = mix(acc, getCrc.##)
    acc = mix(acc, getExtra.##)
    acc = mix(acc, getMethod.##)
    acc = mix(acc, getName.##)
    acc = mix(acc, getSize.##)
    acc = mixLast(acc, getTime.##)
    finalizeHash(acc, 8)
  }

  def isDirectory(): Boolean = _name.endsWith("/")

  def setComment(comment: String): Unit = { _comment = comment }
  def setCompressedSize(csize: Long): Unit = { _csize = csize }
  def setCrc(crc: Long): Unit = { _crc = crc }
  def setExtra(extra: Array[Byte]): Unit = { _extra = extra }
  def setMethod(method: Int): Unit = { _method = method }
  def setSize(size: Long): Unit = { _size = size }
  def setTime(time: Long): Unit = { _time = time }
  override def toString(): String = _name

}
