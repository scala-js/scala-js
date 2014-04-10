package scala.scalajs.tools.io

import java.io.Writer

trait VirtualFileWriter {
  def contentWriter: Writer
}

trait VirtualJSFileWriter extends VirtualFileWriter {
  def sourceMapWriter: Writer
}

trait VirtualScalaJSPackfileWriter extends VirtualJSFileWriter {
  def packInfoWriter: Writer
}
