package scala.scalajs.tools.io

import java.io.InputStream

/** A virtual input file.
 */
trait VirtualFile {
  /** Name of the file, excluding parent directory and including extension. */
  def name: String

  /** Returns the content of the file. */
  def content: String

  /** Returns the lines in the content.
   *  Lines do not contain the new line characters.
   */
  def readLines(): List[String] = IO.readLines(content)

  /** Optionally returns an implementation-dependent "version" token.
   *  Versions are compared with ==.
   *  If non-empty, a different version must be returned when the content
   *  changes. It should be equal if the content has not changed, but it is
   *  not mandatory.
   *  Such a token can be used by caches: the file need not be read and
   *  processed again if its version has not changed.
   */
  def version: Option[Any] = None
}

object VirtualFile {
  def empty(name: String): VirtualFile =
    new MemVirtualFile(name).withVersion(Some(name))
}

/** A virtual input file which contains JavaScript code.
 *  It may have a source map associated with it.
 */
trait VirtualJSFile extends VirtualFile {
  /** Optionally, content of the source map file associated with this
   *  JavaScript source.
   */
  def sourceMap: Option[String] = None
}

object VirtualJSFile {
  def empty(name: String): VirtualJSFile =
    new MemVirtualJSFile(name).withVersion(Some(name))
}

/** A virtual JavaScript input file which was emitted by Scala.js as a
 *  "classfile".
 *  It has an info file associated with it.
 */
trait VirtualScalaJSClassfile extends VirtualJSFile {
  /** Content of the info file associated with this classfile. */
  def info: String
}

object VirtualScalaJSClassfile {
  def empty(name: String): VirtualScalaJSClassfile =
    new MemVirtualScalaJSClassfile(name).withVersion(Some(name))
}
