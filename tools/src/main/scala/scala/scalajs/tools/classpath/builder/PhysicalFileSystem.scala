package scala.scalajs.tools.classpath.builder

import scala.scalajs.tools.io._

import scala.collection.immutable.Traversable

import java.io._

/** FileSystem implementation using java.io._ */
trait PhysicalFileSystem extends FileSystem {

  type File = java.io.File

  def isDirectory(f: File): Boolean = f.isDirectory
  def isFile(f: File): Boolean = f.isFile
  def isJSFile(f: File): Boolean = f.isFile && f.getName.endsWith(".js")
  def isIRFile(f: File): Boolean = f.isFile && f.getName.endsWith(".sjsir")
  def isJARFile(f: File): Boolean = f.isFile && f.getName.endsWith(".jar")

  def getName(f: File): String = f.getName
  def getAbsolutePath(f: File): String = f.getAbsolutePath
  def getVersion(f: File): String = f.lastModified.toString

  def listFiles(d: File): Traversable[File] = {
    require(d.isDirectory)
    Option(d.listFiles).map(_.toList).getOrElse {
      throw new IOException(s"Couldn't list files in $d")
    }
  }

  def toJSFile(f: File): VirtualJSFile = FileVirtualJSFile(f)
  def toIRFile(f: File): VirtualScalaJSIRFile = FileVirtualScalaJSIRFile(f)
  def toReader(f: File): Reader =
    new BufferedReader(new FileReader(f))
  def toInputStream(f: File): InputStream =
    new BufferedInputStream(new FileInputStream(f))

}
