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
}
