package scala.scalajs.tools.io

import java.io.Writer

trait VirtualTextFileWriter {
  def contentWriter: Writer
}

trait VirtualJSFileWriter extends VirtualTextFileWriter {
  def sourceMapWriter: Writer
}

trait VirtualScalaJSPackfileWriter extends VirtualJSFileWriter {
  def packInfoWriter: Writer
}
