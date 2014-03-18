/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.tools.io

import scala.language.implicitConversions

import scala.annotation.tailrec

import java.io.{File => JFile, _}

/** Abstraction of a file system.
 *  The "public" APIs in this project that work with files use such a file
 *  system to abstract away from a particular implementation. In particular,
 *  it is possible to declare a custom file system working with virtual
 *  (in-memory) files and directories.
 */
trait FileSystem { fs =>
  /** Representation of a file (e.g., java.io.File or AbstractFile). */
  type File

  /** Name of the file (last part of the path, i.e., after the last '/'). */
  def name(file: File): String

  /** Tests whether the given file has the specified extension.
   *  Extension contain the '.', so a typical value for `ext` would be ".js".
   *  The comparison is case-sensitive.
   */
  def hasExtension(file: File, ext: String): Boolean =
    file.name.endsWith(ext)

  /** Returns a new file with the same parent as the given file but a different
   *  name.
   */
  def withName(file: File, newName: String): File

  /** Returns a new file with the same path as the given file but a different
   *  extension.
   *  Extension contain the '.', so a typical value for `ext` would be ".js".
   *  Precondition: hasExtension(file, oldExt)
   */
  def withExtension(file: File, oldExt: String, newExt: String): File = {
    require(file.hasExtension(oldExt),
        s"File $file does not have extension '$oldExt'")
    withName(file, file.name.stripSuffix(oldExt) + newExt)
  }

  /** Tests whether a file exists. */
  def exists(file: File): Boolean

  /** Returns an input stream so the file can be read */
  def input(file: File): InputStream

  /** Returns an output stream for writing the file */
  def output(file: File): OutputStream

  /** Reads the file and returns a list of its lines. */
  def readLines(file: File): List[String] = {
    val br = new BufferedReader(new InputStreamReader(input(file)))
    try {
      val builder = List.newBuilder[String]
      @tailrec
      def loop(): Unit = {
        val line = br.readLine()
        if (line ne null) {
          builder += line
          loop()
        }
      }
      loop()
      builder.result()
    } finally {
      br.close()
    }
  }

  implicit def fileOps(file: File): FileOps = new FileOps(file)

  class FileOps(file: File) {
    def name: String = fs.name(file)
    def withName(newName: String): File = fs.withName(file, newName)
    def hasExtension(ext: String): Boolean = fs.hasExtension(file, ext)
    def withExtension(oldExt: String, newExt: String): File =
      fs.withExtension(file, oldExt, newExt)
    def exists: Boolean = fs.exists(file)
    def input: InputStream = fs.input(file)
    def output: OutputStream = fs.output(file)

    /** Returns a buffered output stream for writing the file - defaults to out */
    def bufferedOutput: BufferedOutputStream = new BufferedOutputStream(output)
  }
}

object FileSystem {
  /** Default file system implementation using java.io.File. */
  object DefaultFileSystem extends FileSystem {
    type File = JFile

    def name(file: File): String = file.getName()

    def withName(file: File, newName: String): File =
      new JFile(file.getParentFile(), newName)

    def exists(file: File): Boolean =
      file.exists()

    def input(file: File): InputStream =
      new FileInputStream(file)

    def output(file: File): OutputStream =
      new FileOutputStream(file)
  }
}
