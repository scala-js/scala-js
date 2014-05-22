package scala.scalajs.tools.classpath.builder

import scala.scalajs.tools.io._

import scala.collection.immutable.Traversable

import java.io.{InputStream, Reader}

/** Abstraction of a FileSystem, so classpath builders can be used with virtual
 *  file systems
 */
trait FileSystem {

  type File

  def isDirectory(f: File): Boolean
  def isFile(f: File): Boolean
  def isJSFile(f: File): Boolean
  def isIRFile(f: File): Boolean
  def isJARFile(f: File): Boolean

  def getName(f: File): String
  /** A string that uniquely identifies this file's location */
  def getAbsolutePath(f: File): String
  /** A string that identifies the version of a file: If it equals the version
   *  of another file with the same absolute path, the two files must be equal.
   *  This is usually the lastModified date, but ordering is not required
   */
  def getVersion(f: File): String
  /** A string that globally identifies the version of a file: If it equals the
   *  global version of any other file, they must equal.
   */
  def getGlobalVersion(f: File): String =
    CacheUtils.joinVersions(getAbsolutePath(f), getVersion(f))

  /** List files in a directory */
  def listFiles(d: File): Traversable[File]

  def toJSFile(f: File): VirtualJSFile
  def toIRFile(f: File): VirtualScalaJSIRFile
  def toReader(f: File): Reader
  def toInputStream(f: File): InputStream

}
