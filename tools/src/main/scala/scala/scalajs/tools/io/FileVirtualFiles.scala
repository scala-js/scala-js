package scala.scalajs.tools.io

import scala.annotation.tailrec

import java.io._

/** A [[VirtualFile]] implemented by an actual file on the file system. */
class FileVirtualFile(val file: File) extends VirtualFile {
  import FileVirtualFile._

  override def name = file.getName

  override def content: String = readFileToString(file)

  override def version: Option[Any] = {
    if (!file.isFile) None
    else Some(file.lastModified())
  }
}

object FileVirtualFile extends (File => FileVirtualFile) {
  def apply(f: File): FileVirtualFile =
    new FileVirtualFile(f)

  /** Reads the entire content of a file as a UTF-8 string. */
  def readFileToString(file: File): String = {
    val reader = new BufferedReader(new InputStreamReader(
        new FileInputStream(file), "UTF-8"))
    try {
      val buffer = new Array[Char](4096)
      val builder = new StringBuilder
      @tailrec
      def loop(): Unit = {
        val len = reader.read(buffer)
        if (len > 0) {
          builder.appendAll(buffer, 0, len)
          loop()
        }
      }
      loop()
      builder.toString()
    } finally {
      reader.close()
    }
  }

  /** Tests whether the given file has the specified extension.
   *  Extension contain the '.', so a typical value for `ext` would be ".js".
   *  The comparison is case-sensitive.
   */
  def hasExtension(file: File, ext: String): Boolean =
    file.getName.endsWith(ext)

  /** Returns a new file with the same parent as the given file but a different
   *  name.
   */
  def withName(file: File, newName: String): File =
    new File(file.getParentFile(), newName)

  /** Returns a new file with the same path as the given file but a different
   *  extension.
   *  Extension contain the '.', so a typical value for `ext` would be ".js".
   *  Precondition: hasExtension(file, oldExt)
   */
  def withExtension(file: File, oldExt: String, newExt: String): File = {
    require(hasExtension(file, oldExt),
        s"File $file does not have extension '$oldExt'")
    withName(file, file.getName.stripSuffix(oldExt) + newExt)
  }
}

class FileVirtualJSFile(f: File) extends FileVirtualFile(f) with VirtualJSFile {
  import FileVirtualFile._

  override def sourceMap: Option[String] = {
    val f = withExtension(file, ".js", ".js.map")
    if (f.exists) Some(readFileToString(f))
    else None
  }
}

object FileVirtualJSFile extends (File => FileVirtualJSFile) {
  def apply(f: File): FileVirtualJSFile =
    new FileVirtualJSFile(f)
}

class FileVirtualScalaJSClassfile(f: File)
    extends FileVirtualJSFile(f) with VirtualScalaJSClassfile {
  import FileVirtualFile._

  override def info: String =
    readFileToString(withExtension(file, ".js", ".sjsinfo"))
}

object FileVirtualScalaJSClassfile extends (File => FileVirtualScalaJSClassfile) {
  def apply(f: File): FileVirtualScalaJSClassfile =
    new FileVirtualScalaJSClassfile(f)
}
