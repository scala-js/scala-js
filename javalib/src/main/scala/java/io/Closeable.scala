package java.io

/** Note that Closeable doesn't extend AutoCloseable for Java6 compat */
trait Closeable {
  def close(): Unit
}
