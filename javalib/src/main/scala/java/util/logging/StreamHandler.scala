package java.util.logging

import java.io.OutputStream

class StreamHandler(private[this] val _out: OutputStream,
    private[this] val _formatter: Formatter) extends Handler {

  // Required by javadoc but it is unspecified what to do if formatter is null
  def this() = this(null, null)

  protected var out:Option[OutputStream] = Option(_out)
  private[this] val formatter:Option[Formatter] = Option(_formatter)
  private[this] var headWritten:Boolean = false

  protected def setOutputStream(out: OutputStream):Unit = {
    // Required by javadocs
    for {
      o <- this.out
      f <- formatter
    } {
      o.write(f.getTail(this).getBytes(getEncoding))
    }
    this.out.foreach { o =>
      o.flush()
      o.close()
    }
    this.headWritten = false
    this.out = Option(out)
  }

  // Mentioned as part of StreamHandler javadocs but it doesn't specify behavior
  override def setEncoding(encoding: String):Unit = super.setEncoding(encoding)

  private def write(c: Formatter => String): Unit = {
    for {
      o <- out
      // The javadocs don't specify what to do if the formatter is null
      f <- formatter
      e <- encodingOrDefault
      t = c(f)
    } {
      o.write(t.getBytes(e))
      flush()
    }
  }

  protected def writeHeader():Unit = {
    if (!headWritten) {
      write(_.getHead(this))
      headWritten = true
    }
  }

  protected def writeTail():Unit = {
    write(_.getTail(this))
  }

  override def publish(record: LogRecord): Unit = {
    writeHeader()
    for {
      o <- out
      if isLoggable(record)
      // The javadocs don't specify what to do if the formatter is null
      f <- formatter
      e <- encodingOrDefault
      t = f.format(record)
    } {
      o.write(t.getBytes(e))
      flush()
    }
  }

  override def isLoggable(record:LogRecord):Boolean =
    out.isDefined && record != null && super.isLoggable(record)

  override def flush(): Unit = out.foreach(_.flush())

  override def close(): Unit = {
    // Required by javadocs
    writeHeader()
    writeTail()
    this.out.foreach { o =>
      o.flush()
      o.close()
    }

  }
}
