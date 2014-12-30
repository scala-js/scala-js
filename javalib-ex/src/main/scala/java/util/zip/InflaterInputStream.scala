package java.util.zip

import java.io._

/** Dummy implementation of InflatorInputStream. Does not do anything. */
class InflaterInputStream(in: InputStream) extends FilterInputStream(in) {

  // Not implemented:
  // def this(in: InputStream, inf: Inflater)
  // def this(in: InputStream, inf: Inflater, size: Int)
  // protected var buf: Array[Byte]
  // protected var inf: Inflater
  // protected var len: Int

  override def markSupported(): Boolean = false
  override def mark(readlimit: Int): Unit = {}
  override def reset(): Unit = throw new IOException("Reset not supported")

}
