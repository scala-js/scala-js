package java.util.logging

// Unfortunately the spec require the use of System.err
class ConsoleHandler extends StreamHandler(System.err, new SimpleFormatter) {

  // Overriden on the javadocs but no comments about differences
  override def publish(record: LogRecord): Unit =
    super.publish(record)

  override def close(): Unit = {
    // Required by javadocs
    writeHeader()
    writeTail()
    out.foreach { o =>
      // Just flush, no closing
      o.flush()
    }
  }
}
