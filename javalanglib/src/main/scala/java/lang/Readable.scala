package java.lang

import java.nio.CharBuffer

trait Readable {
  def read(cb: CharBuffer): Int
}
