package org.scalajs.io

import java.io._
import java.net.URI

/** A virtual input file.
 */
trait VirtualFile {
  /** Path of the file, including everything.
   *  Unique if possible (used for lookup). */
  def path: String

  /** Name of the file/writer, including extension */
  def name: String = VirtualFile.nameFromPath(path)

  /** Optionally returns an implementation-dependent "version" token.
   *  Versions are compared with ==.
   *  If non-empty, a different version must be returned when the content
   *  changes. It should be equal if the content has not changed, but it is
   *  not mandatory.
   *  Such a token can be used by caches: the file need not be read and
   *  processed again if its version has not changed.
   */
  def version: Option[String] = None

  override def toString(): String = {
    val className = getClass.getName
    val shortClassName = className.substring(className.lastIndexOf('.') + 1)
    shortClassName + "(" + path + ")"
  }
}

object VirtualFile {
  /** Splits at the last slash and returns remainder */
  def nameFromPath(path: String): String = {
    val pos = path.lastIndexOf('/')
    if (pos == -1) path
    else path.substring(pos + 1)
  }
}

trait RelativeVirtualFile extends VirtualFile {
  /** Relative path with respect to some container.
   *
   *  The container depends on the context in which this [[RelativeVirtualFile]]
   *  is retrieved. A good example is the [[VirtualJarFile]] where the relative
   *  path is the path inside the Jar.
   */
  def relativePath: String
}

/** A virtual input file.
 */
trait VirtualTextFile extends VirtualFile {
  /** Returns the content of the file. */
  def content: String

  /** Returns a new Reader of the file. */
  def reader: Reader = new StringReader(content)

  /** Returns the lines in the content.
   *  Lines do not contain the new line characters.
   */
  def readLines(): List[String] = IO.readLines(reader)
}

object VirtualTextFile {
  def empty(path: String): VirtualTextFile =
    new MemVirtualTextFile(path)
}

trait WritableVirtualTextFile extends VirtualTextFile {
  def contentWriter: Writer
}

/** A virtual binary input file.
 */
trait VirtualBinaryFile extends VirtualFile {
  /** Returns the content of the file. */
  def content: Array[Byte]

  /** Returns a new InputStream of the file. */
  def inputStream: InputStream = new ByteArrayInputStream(content)
}

trait WritableVirtualBinaryFile extends VirtualBinaryFile {
  def outputStream: OutputStream
}

/** A virtual input file which contains JavaScript code.
 *  It may have a source map associated with it.
 */
trait VirtualJSFile extends VirtualTextFile {
  /** Optionally, content of the source map file associated with this
   *  JavaScript source.
   */
  def sourceMap: Option[String] = None
}

object VirtualJSFile {
  def empty(path: String): VirtualJSFile =
    new MemVirtualJSFile(path).withVersion(Some(path))
}

trait WritableVirtualJSFile extends WritableVirtualTextFile with VirtualJSFile {
  def sourceMapWriter: Writer
}

/** A virtual file container.
 *
 *  This is a generic virtual container for embedded virtual files, especially
 *  one found on a classpath such as a jar.
 */
trait VirtualFileContainer extends VirtualFile {
  /** Lists the entries of this container that satisfy a given predicate.
   *
   *  @param p
   *    Predicate on the relative path of files to select.
   *  @param makeResult
   *    Function building an element of the result list for an entry, given
   *    its relative path an `InputStream` of the content. `makeResult` may
   *    `close()` the input stream, but it is not mandatory. In any case, the
   *    input stream cannot be used after `makeResult` returns.
   */
  def listEntries[T](p: String => Boolean)(
      makeResult: (String, InputStream) => T): List[T]
}
