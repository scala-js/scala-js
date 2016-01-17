package org.scalajs.core.tools.io

import java.io._
import java.net.URI

/** A [[VirtualFile]] implemented by an actual file on the file system. */
class FileVirtualFile(val file: File) extends VirtualFile {
  import FileVirtualFile._

  override def path: String = file.getPath

  override def name: String = file.getName

  override def version: Option[String] = {
    if (!file.isFile) None
    else Some(file.lastModified.toString)
  }

  override def exists: Boolean = file.exists

  override def toURI: URI = file.toURI
}

object FileVirtualFile extends (File => FileVirtualFile) {
  def apply(f: File): FileVirtualFile =
    new FileVirtualFile(f)

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

/** A [[VirtualTextFile]] implemented by an actual file on the file system. */
class FileVirtualTextFile(f: File) extends FileVirtualFile(f)
                                      with VirtualTextFile {
  import FileVirtualTextFile._

  override def content: String = readFileToString(file)
  override def reader: Reader = new BufferedReader(new FileReader(f))
}

object FileVirtualTextFile extends (File => FileVirtualTextFile) {
  def apply(f: File): FileVirtualTextFile =
    new FileVirtualTextFile(f)

  /** Reads the entire content of a file as a UTF-8 string. */
  def readFileToString(file: File): String = {
    val stream = new FileInputStream(file)
    try IO.readInputStreamToString(stream)
    finally stream.close()
  }
}

trait WritableFileVirtualTextFile extends FileVirtualTextFile
                                     with WritableVirtualTextFile {
  override def contentWriter: Writer = {
    new BufferedWriter(new OutputStreamWriter(
        new FileOutputStream(file), "UTF-8"))
  }
}

object WritableFileVirtualTextFile {
  def apply(f: File): WritableFileVirtualTextFile =
    new FileVirtualTextFile(f) with WritableFileVirtualTextFile
}

/** A [[VirtualBinaryFile]] implemented by an actual file on the file system. */
class FileVirtualBinaryFile(f: File) extends FileVirtualFile(f)
                                        with VirtualBinaryFile {
  import FileVirtualBinaryFile._

  override def inputStream: InputStream =
    new BufferedInputStream(new FileInputStream(file))

  override def content: Array[Byte] =
    readFileToByteArray(file)
}

object FileVirtualBinaryFile extends (File => FileVirtualBinaryFile) {
  def apply(f: File): FileVirtualBinaryFile =
    new FileVirtualBinaryFile(f)

  /** Reads the entire content of a file as byte array. */
  def readFileToByteArray(file: File): Array[Byte] = {
    val stream = new FileInputStream(file)
    try IO.readInputStreamToByteArray(stream)
    finally stream.close()
  }
}

trait WritableFileVirtualBinaryFile extends FileVirtualBinaryFile
                                       with WritableVirtualBinaryFile {
  override def outputStream: OutputStream =
    new BufferedOutputStream(new FileOutputStream(file))
}

object WritableFileVirtualBinaryFile {
  def apply(f: File): WritableFileVirtualBinaryFile =
    new FileVirtualBinaryFile(f) with WritableFileVirtualBinaryFile
}

class FileVirtualJSFile(f: File) extends FileVirtualTextFile(f)
                                    with VirtualJSFile {
  import FileVirtualFile._
  import FileVirtualTextFile._

  val sourceMapFile: File = withExtension(file, ".js", ".js.map")

  override def sourceMap: Option[String] = {
    if (sourceMapFile.exists) Some(readFileToString(sourceMapFile))
    else None
  }
}

object FileVirtualJSFile extends (File => FileVirtualJSFile) {
  def apply(f: File): FileVirtualJSFile =
    new FileVirtualJSFile(f)

  def relative(f: File,
      relPath: String): FileVirtualJSFile with RelativeVirtualFile = {
    new FileVirtualJSFile(f) with RelativeVirtualFile {
      def relativePath: String = relPath
    }
  }
}

trait WritableFileVirtualJSFile extends FileVirtualJSFile
                                   with WritableFileVirtualTextFile
                                   with WritableVirtualJSFile {

  override def sourceMapWriter: Writer = {
    new BufferedWriter(new OutputStreamWriter(
        new FileOutputStream(sourceMapFile), "UTF-8"))
  }
}

object WritableFileVirtualJSFile {
  def apply(f: File): WritableFileVirtualJSFile =
    new FileVirtualJSFile(f) with WritableFileVirtualJSFile
}

class FileVirtualScalaJSIRFile(f: File)
    extends FileVirtualBinaryFile(f) with VirtualSerializedScalaJSIRFile

object FileVirtualScalaJSIRFile extends (File => FileVirtualScalaJSIRFile) {
  import FileVirtualFile._

  def apply(f: File): FileVirtualScalaJSIRFile =
    new FileVirtualScalaJSIRFile(f)

  def relative(f: File,
      relPath: String): FileVirtualScalaJSIRFile with RelativeVirtualFile = {
    new FileVirtualScalaJSIRFile(f) with RelativeVirtualFile {
      def relativePath: String = relPath
    }
  }

  def isScalaJSIRFile(file: File): Boolean =
    hasExtension(file, ".sjsir")
}
