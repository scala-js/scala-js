package scala.scalajs.tools.io

import scala.annotation.tailrec

import java.io._

object IO {
  /** Returns the lines in an input stream.
   *  Lines do not contain the new line characters.
   */
  def readLines(stream: InputStream): List[String] =
    readLines(new InputStreamReader(stream))

  /** Returns the lines in a string.
   *  Lines do not contain the new line characters.
   */
  def readLines(content: String): List[String] =
    readLines(new StringReader(content))

  /** Returns the lines in a reader.
   *  Lines do not contain the new line characters.
   */
  def readLines(reader: Reader): List[String] = {
    val br = new BufferedReader(reader)
    try {
      val builder = List.newBuilder[String]
      @tailrec
      def loop(): Unit = {
        val line = br.readLine()
        if (line ne null) {
          builder += line
          loop()
        }
      }
      loop()
      builder.result()
    } finally {
      br.close()
    }
  }

  /** Reads the entire content of a reader as a string. */
  def readReaderToString(reader: Reader): String = {
    val buffer = new Array[Char](4096)
    val builder = new StringBuilder
    @tailrec
    def loop(): Unit = {
      val len = reader.read(buffer)
      if (len > 0) {
        builder.appendAll(buffer, 0, len)
        loop()
      }
    }
    loop()
    builder.toString()
  }

  /** Reads the entire content of an input stream as a UTF-8 string. */
  def readInputStreamToString(stream: InputStream): String = {
    val reader = new BufferedReader(new InputStreamReader(stream, "UTF-8"))
    readReaderToString(reader)
  }

  /** Reads the entire content of an input stream as a byte array. */
  def readInputStreamToByteArray(stream: InputStream): Array[Byte] = {
    val builder = new ByteArrayOutputStream()
    val buffer = new Array[Byte](4096)
    @tailrec
    def loop(): Unit = {
      val size = stream.read(buffer)
      if (size > 0) {
        builder.write(buffer, 0, size)
        loop()
      }
    }
    loop()
    builder.toByteArray()
  }
}
