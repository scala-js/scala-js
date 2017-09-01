package java.io

trait Closeable extends AutoCloseable {
  def close(): Unit
}
