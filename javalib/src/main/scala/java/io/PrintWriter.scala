package java.io

import java.util.Formatter

class PrintWriter(protected[io] var out: Writer,
    autoFlush: Boolean) extends Writer {

  def this(out: Writer) = this(out, false)

  def this(out: OutputStream, autoFlush: Boolean) =
    this(new OutputStreamWriter(out), autoFlush)
  def this(out: OutputStream) =
    this(out, false)

  /* The following constructors, although implemented, will not link, since
   * File, FileOutputStream and BufferedOutputStream are not implemented.
   * They're here just in case a third-party library on the classpath
   * implements those.
   */
  def this(file: File) =
    this(new BufferedOutputStream(new FileOutputStream(file)))
  def this(file: File, csn: String) =
    this(new OutputStreamWriter(new BufferedOutputStream(
        new FileOutputStream(file)), csn))
  def this(fileName: String) = this(new File(fileName))
  def this(fileName: String, csn: String) = this(new File(fileName), csn)

  private var closed: Boolean = false
  private var errorFlag: Boolean = false

  def flush(): Unit =
    ensureOpenAndTrapIOExceptions(out.flush())

  def close(): Unit = trapIOExceptions {
    if (!closed) {
      flush()
      closed = true
      out.close()
    }
  }

  def checkError(): Boolean = {
    if (closed) {
      /* Just check the error flag.
       * Common sense would tell us to look at the underlying writer's
       * checkError() result too (like we do in the not closed case below).
       * But the JDK does not behave like that. So we don't either.
       */
      errorFlag
    } else {
      flush()
      /* If the underlying writer is also a PrintWriter, we also check its
       * checkError() result. This is not clearly specified by the JavaDoc,
       * but, experimentally, the JDK seems to behave that way.
       */
      errorFlag || (out match {
        case out: PrintWriter => out.checkError()
        case _                => false
      })
    }
  }

  protected[io] def setError(): Unit = errorFlag = true
  protected[io] def clearError(): Unit = errorFlag = false

  override def write(c: Int): Unit =
    ensureOpenAndTrapIOExceptions(out.write(c))

  override def write(buf: Array[Char], off: Int, len: Int): Unit =
    ensureOpenAndTrapIOExceptions(out.write(buf, off, len))

  override def write(buf: Array[Char]): Unit =
    ensureOpenAndTrapIOExceptions(out.write(buf))

  override def write(s: String, off: Int, len: Int): Unit =
    ensureOpenAndTrapIOExceptions(out.write(s, off, len))

  override def write(s: String): Unit =
    ensureOpenAndTrapIOExceptions(out.write(s))

  def print(b: Boolean): Unit     = write(String.valueOf(b))
  def print(c: Char): Unit        = write(c)
  def print(i: Int): Unit         = write(String.valueOf(i))
  def print(l: Long): Unit        = write(String.valueOf(l))
  def print(f: Float): Unit       = write(String.valueOf(f))
  def print(d: Double): Unit      = write(String.valueOf(d))
  def print(s: Array[Char]): Unit = write(s)
  def print(s: String): Unit      = write(if (s == null) "null" else s)
  def print(obj: AnyRef): Unit    = write(String.valueOf(obj))

  def println(): Unit = {
    write('\n') // In Scala.js the line separator is always LF
    if (autoFlush)
      flush()
  }

  def println(b: Boolean): Unit     = { print(b); println() }
  def println(c: Char): Unit        = { print(c); println() }
  def println(i: Int): Unit         = { print(i); println() }
  def println(l: Long): Unit        = { print(l); println() }
  def println(f: Float): Unit       = { print(f); println() }
  def println(d: Double): Unit      = { print(d); println() }
  def println(s: Array[Char]): Unit = { print(s); println() }
  def println(s: String): Unit      = { print(s); println() }
  def println(obj: AnyRef): Unit    = { print(obj); println() }

  def printf(fmt: String, args: Array[Object]): PrintWriter =
    format(fmt, args)

  // Not implemented:
  //def printf(l: java.util.Locale, fmt: String, args: Array[Object]): PrintWriter = ???

  def format(fmt: String, args: Array[Object]): PrintWriter = {
    new Formatter(this).format(fmt, args)
    if (autoFlush)
      flush()
    this
  }

  // Not implemented:
  //def format(l: java.util.Locale, fmt: String, args: Array[Object]): PrintWriter = ???

  override def append(csq: CharSequence): PrintWriter = {
    super.append(csq)
    this
  }

  override def append(csq: CharSequence, start: Int, end: Int): PrintWriter = {
    super.append(csq, start, end)
    this
  }

  override def append(c: Char): PrintWriter = {
    super.append(c)
    this
  }

  @inline private[this] def trapIOExceptions(body: => Unit): Unit = {
    try {
      body
    } catch {
      case _: IOException => setError()
    }
  }

  @inline private[this] def ensureOpenAndTrapIOExceptions(body: => Unit): Unit = {
    if (closed) setError()
    else trapIOExceptions(body)
  }
}
