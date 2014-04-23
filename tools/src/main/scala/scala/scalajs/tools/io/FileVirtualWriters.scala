package scala.scalajs.tools.io

import java.io._

class FileVirtualTextFileWriter(val file: File) extends VirtualTextFileWriter {

  private[this] var _contentWriter: Writer = null

  override def contentWriter: Writer = {
    if (_contentWriter != null) _contentWriter
    else {
      _contentWriter = new OutputStreamWriter(
          new BufferedOutputStream(new FileOutputStream(file)), "UTF-8")
      _contentWriter
    }
  }

  def close(): Unit = if (_contentWriter != null) _contentWriter.close()

}

class FileVirtualJSFileWriter(f: File) extends FileVirtualTextFileWriter(f)
                                          with VirtualJSFileWriter {

  import FileVirtualFile.withExtension

  val sourceMapFile = withExtension(file, ".js", ".js.map")

  private[this] var _sourceMapWriter: Writer = null

  override def sourceMapWriter: Writer = {
    if (_sourceMapWriter != null) _sourceMapWriter
    else {
      _sourceMapWriter = new OutputStreamWriter(
          new BufferedOutputStream(
              new FileOutputStream(sourceMapFile)), "UTF-8")
      _sourceMapWriter
    }
  }

  override def close(): Unit = {
    super.close()
    if (_sourceMapWriter != null) _sourceMapWriter.close()
  }

}

class FileVirtualScalaJSPackfileWriter(f: File) extends
    FileVirtualJSFileWriter(f) with VirtualScalaJSPackfileWriter {

  import FileVirtualFile.withExtension

  val packInfoFile = withExtension(file, ".js", ".sjspack")

  private[this] var _packInfoWriter: Writer = null

  override def packInfoWriter: Writer = {
    if (_packInfoWriter != null) _packInfoWriter
    else {
      _packInfoWriter = new OutputStreamWriter(
          new BufferedOutputStream(
              new FileOutputStream(packInfoFile)), "UTF-8")
      _packInfoWriter
    }
  }

  override def close(): Unit = {
    super.close()
    if (_packInfoWriter != null) _packInfoWriter.close()
  }

}
