/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2018, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package org.scalajs.linker.irio

import java.io._

import org.scalajs.ir

import org.scalajs.io._

/** A virtual file containing Scala.js IR.
 *
 *  The main difference compared to using individual files
 *  (that are extracted beforehand) is that the fileset can be versioned at a
 *  higher level: the container needs to change its version when any of the
 *  files change. Therefore, the entire extraction process can be cached.
 */
trait ScalaJSIRContainer extends VirtualFile {
  /** All the `*.sjsir` files in this container.
   *
   *  It is up to the implementation whether these files are read lazily or not.
   */
  def sjsirFiles: List[VirtualScalaJSIRFile]
}

object ScalaJSIRContainer {
  def sjsirFilesIn(
      container: VirtualFileContainer): List[VirtualScalaJSIRFile] = {
    container.listEntries(_.endsWith(".sjsir")) { (relPath, stream) =>
      val file = new EntryIRFile(container.path, relPath)
      file.content = IO.readInputStreamToByteArray(stream)
      file.version = container.version
      file
    }
  }

  private class EntryIRFile(outerPath: String, relativePath: String)
      extends MemVirtualSerializedScalaJSIRFile(s"$outerPath:$relativePath", relativePath)
      with VirtualScalaJSIRFile
}

/** A virtual Scala.js IR file.
 *  It contains the class info and the IR tree.
 */
trait VirtualScalaJSIRFile extends VirtualFile with ScalaJSIRContainer {
  /** Entry points information for this file. */
  def entryPointsInfo: ir.EntryPointsInfo =
    ir.EntryPointsInfo.forClassDef(tree)

  /** IR Tree of this file. */
  def tree: ir.Trees.ClassDef

  /** The path of this IR relative to its classpath root. */
  def relativePath: String

  def sjsirFiles: List[VirtualScalaJSIRFile] = this :: Nil
}

/** Base trait for virtual Scala.js IR files that are serialized as binary file.
 */
trait VirtualSerializedScalaJSIRFile
    extends VirtualBinaryFile with VirtualScalaJSIRFile {

  override def entryPointsInfo: ir.EntryPointsInfo = {
    // Overridden to read only the necessary parts
    withInputStream(ir.Serializers.deserializeEntryPointsInfo)
  }

  override def tree: ir.Trees.ClassDef =
    withInputStream(ir.Serializers.deserialize)

  @inline
  private def withInputStream[A](f: InputStream => A): A = {
    val stream = inputStream
    try {
      f(stream)
    } catch {
      case e: ir.IRVersionNotSupportedException =>
        throw new ir.IRVersionNotSupportedException(e.version, e.supported,
            s"Failed to deserialize a file compiled with Scala.js ${e.version}" +
            s" (supported: ${e.supported.mkString(", ")}): $path", e)

      case e: IOException =>
        throw new IOException(s"Failed to deserialize $path", e)
    } finally {
      stream.close()
    }
  }
}
