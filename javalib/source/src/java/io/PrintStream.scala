package java.io

class PrintStream(_out: OutputStream, autoFlush: Boolean, ecoding: String)
    extends FilterOutputStream(_out) with Appendable {
  import java.util.Locale

  def this(out: OutputStream) = this(out, false, "")
  def this(out: OutputStream, autoFlush: Boolean) = this(out, autoFlush, "")

  override def write(b: Int) = {
    _out.write(b)
    if (autoFlush && b == 10) flush()
  }

  def append(c: Char) = this
  def append(csq: CharSequence) = this
  def append(csq: CharSequence, start: Int, end: Int) = this

  var hasError = false
  def checkError() = hasError
  def setError() { hasError = true }
  def clearError() { hasError = false }

  def print(b: Boolean): Unit = print(b.toString)
  def print(c: Char): Unit = print(c.toString)
  def print(i: Int): Unit = print(i.toString)
  def print(l: Long): Unit = print(l.toString)
  def print(f: Float): Unit = print(f.toString)
  def print(d: Double): Unit = print(d.toString)
  def print(s: Array[Char]): Unit = print("character array")
  def print(s: String): Unit = if (s eq null) print("null") else writeString(s)
  def print(o: Object): Unit = if (o eq null) print("null") else print(o.toString)

  protected def writeString(s: String) = {
    val bytes = new Array[Byte](s.length)
    for (i <- 0 until s.length)
      bytes(i) = s.charAt(i).toByte
    write(bytes)
  }

  def println(): Unit = write(10)
  def println(x: Boolean): Unit = { print(x); println() }
  def println(x: Char): Unit = { print(x); println() }
  def println(x: Int): Unit = { print(x); println() }
  def println(x: Long): Unit = { print(x); println() }
  def println(x: Float): Unit = { print(x); println() }
  def println(x: Double): Unit = { print(x); println() }
  def println(x: String): Unit = { print(x); println() }
  def println(x: Object): Unit = { print(x); println() }

  def printf(format: String, args: Array[Object]): Unit = print("printf")
  def printf(l: Locale, format: String, args: Array[Object]): Unit = print("printf")
  def format(format: String, args: Array[Object]): Unit = print("printf")
  def format(l: Locale, format: String, args: Array[Object]): Unit = print("printf")
}
