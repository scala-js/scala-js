package org.scalajs.core.tools.io

import java.io._
import java.net.URI
import java.util.zip.{ZipInputStream, ZipEntry}

import org.scalajs.core.ir

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

  /** Whether this file exists. Reading a non-existent file may fail */
  def exists: Boolean

  /** URI for this virtual file */
  def toURI: URI = {
    new URI(
        "virtualfile", // Pseudo-Scheme
        path,          // Scheme specific part
        null           // Fragment
    )
  }

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

/** A virtual file containing Scala.js IR.
 *
 *  This can be a [[VirtualScalaJSIRFile]] (with [[RelativeVirtualFile]]) or a
 *  [[VirtualFileContainer]].
 *
 *  The main difference compared to using individual files
 *  (that are extracted beforehand) is that the fileset can be versioned at a
 *  higher level: the container needs to change its version when any of the
 *  files change. Therefore, the entire extraction process can be cached.
 */
trait ScalaJSIRContainer extends VirtualFile {
  def sjsirFiles: List[VirtualRelativeScalaJSIRFile]
}

/** A virtual Scala.js IR file.
 *  It contains the class info and the IR tree.
 */
trait VirtualScalaJSIRFile extends VirtualFile {
  /** Class info of this file. */
  def info: ir.Infos.ClassInfo =
    infoAndTree._1

  /** IR Tree of this file. */
  def tree: ir.Trees.ClassDef =
    infoAndTree._2

  /** Class info and IR tree of this file. */
  def infoAndTree: (ir.Infos.ClassInfo, ir.Trees.ClassDef)
}

trait VirtualRelativeScalaJSIRFile extends VirtualScalaJSIRFile
    with RelativeVirtualFile with ScalaJSIRContainer {
  def sjsirFiles: List[VirtualRelativeScalaJSIRFile] = this :: Nil
}

/** Base trait for virtual Scala.js IR files that are serialized as binary file.
 */
trait VirtualSerializedScalaJSIRFile extends VirtualBinaryFile with VirtualScalaJSIRFile {
  /** Class info of this file. */
  override def info: ir.Infos.ClassInfo = {
    // Overridden to read only the necessary parts
    val stream = inputStream
    try {
      ir.InfoSerializers.deserialize(stream)
    } catch {
      case e: ir.IRVersionNotSupportedException =>
        throw new ir.IRVersionNotSupportedException(e.version, e.supported,
            s"Failed to deserialize info of file compiled with Scala.js ${e.version}" +
            s" (supported: ${e.supported.mkString(", ")}): $path", e)

      case e: IOException =>
        throw new IOException(s"Failed to deserialize info of $path", e)
    } finally {
      stream.close()
    }
  }

  /** Class info and IR tree of this file. */
  override def infoAndTree: (ir.Infos.ClassInfo, ir.Trees.ClassDef) = {
    val stream = inputStream
    try {
      val (version, info) = ir.InfoSerializers.deserializeWithVersion(stream)
      val tree = ir.Serializers.deserialize(stream, version)
      (info, tree)
    } catch {
      case e: IOException =>
        throw new IOException(s"Failed to deserialize $path", e)
    } finally {
      stream.close()
    }
  }
}

/** A virtual file container.
 *
 *  This is a generic virtual container for embedded virtual files, especially
 *  one found on a classpath such as a jar, and containing `.sjsir` files.
 */
trait VirtualFileContainer extends ScalaJSIRContainer {
  import VirtualFileContainer._

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

  /** All the `*.sjsir` files in this container.
   *
   *  It is up to the implementation whether these files are read lazily or not.
   *  The default implementation reads them into memory.
   *
   *  Depending on the implementation, calling `sjsirFiles` might be more
   *  efficient than using `listEntries` with a predicate
   *  `_.endsWith(".sjsir")`.
   */
  def sjsirFiles: List[VirtualRelativeScalaJSIRFile] = {
    listEntries(_.endsWith(".sjsir")) { (relPath, stream) =>
      val file = new EntryIRFile(path, relPath)
      file.content = IO.readInputStreamToByteArray(stream)
      file.version = version
      file
    }
  }

  def jsFiles: List[VirtualJSFile with RelativeVirtualFile] = {
    listEntries(_.endsWith(".js")) { (relPath, stream) =>
      val file = new EntryJSFile(path, relPath)
      file.content = IO.readInputStreamToString(stream)
      file.version = version
      file
    }
  }
}

private object VirtualFileContainer {
  private class EntryIRFile(outerPath: String, val relativePath: String)
      extends MemVirtualSerializedScalaJSIRFile(s"$outerPath:$relativePath")
      with VirtualRelativeScalaJSIRFile

  private class EntryJSFile(outerPath: String, val relativePath: String)
      extends MemVirtualJSFile(s"$outerPath:$relativePath")
      with RelativeVirtualFile
}

/** A virtual jar file. */
trait VirtualJarFile extends VirtualFileContainer with VirtualBinaryFile {
  import VirtualJarFile._

  def listEntries[T](p: String => Boolean)(
      makeResult: (String, InputStream) => T): List[T] = {
    val stream = new ZipInputStream(inputStream)
    try {
      val streamIgnoreClose = new IgnoreCloseFilterInputStream(stream)
      Iterator.continually(stream.getNextEntry())
        .takeWhile(_ != null)
        .filter(entry => p(entry.getName))
        .map(entry => makeResult(entry.getName, streamIgnoreClose))
        .toList
    } finally {
      stream.close()
    }
  }
}

private object VirtualJarFile {
  private final class IgnoreCloseFilterInputStream(in: InputStream)
      extends FilterInputStream(in) {

    override def close(): Unit = ()
  }
}
