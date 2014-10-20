package scala.scalajs.tools.classpath.builder

import scala.scalajs.tools.io._

import scala.scalajs.js

import scala.collection.immutable.Traversable

import java.io._

/** FileSystem implementation using Node.js */
trait NodeFileSystem extends FileSystem {

  import NodeFileSystem.fs

  type File = String

  private def stats(f: String) = fs.statSync(f)

  val DummyVersion: String = "DUMMY_FILE"

  def isDirectory(f: String): Boolean =
    stats(f).isDirectory().asInstanceOf[Boolean]

  def isFile(f: String): Boolean =
    stats(f).isFile().asInstanceOf[Boolean]

  def isJSFile(f: String): Boolean =
    isFile(f) && f.endsWith(".js")

  def isIRFile(f: String): Boolean =
    isFile(f) && f.endsWith(".sjsir")

  def isJARFile(f: String): Boolean =
    isFile(f) && f.endsWith(".jar")

  def exists(f: String): Boolean =
    fs.existsSync(f).asInstanceOf[Boolean]

  def getName(f: String): String =
    VirtualFile.nameFromPath(f)

  def getAbsolutePath(f: String): String =
    fs.realpathSync(f).asInstanceOf[String]

  def getVersion(f: String): String =
    stats(f).mtime.asInstanceOf[js.Date].getTime.toString

  def listFiles(d: String): Traversable[String] = {
    require(isDirectory(d))
    val prefix = if (d.endsWith("/")) d else d + "/"

    fs.readdirSync(d).asInstanceOf[js.Array[String]].toList.map(prefix + _)
  }

  def toJSFile(f: String): VirtualJSFile = new NodeVirtualJSFile(f)
  def toIRFile(f: String): VirtualScalaJSIRFile = new NodeVirtualScalaJSIRFile(f)
  def toReader(f: String): Reader =
    new NodeVirtualTextFile(f).reader
  def toInputStream(f: String): InputStream =
    new NodeVirtualBinaryFile(f).inputStream

}

private object NodeFileSystem {
  private val fs = js.Dynamic.global.require("fs")
}
