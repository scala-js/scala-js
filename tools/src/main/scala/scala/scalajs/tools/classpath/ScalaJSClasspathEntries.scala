/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.tools.classpath

import scala.annotation.tailrec

import java.io._
import java.util.zip._

import scala.collection.mutable

import scala.scalajs.tools.io._

final case class ScalaJSClasspathEntries(
    coreJSLibFile: VirtualJSFile,
    coreInfoFiles: Seq[VirtualFile],
    classFiles: Seq[VirtualScalaJSClassfile],
    otherJSFiles: Seq[VirtualJSFile] = Nil
)

object ScalaJSClasspathEntries {
  class Builder {
    private var coreJSLibFile: Option[VirtualJSFile] = None
    private val coreInfoFiles = mutable.ListBuffer.empty[VirtualFile]
    private val classFiles = mutable.Map.empty[String, VirtualScalaJSClassfile]
    private val otherJSFiles = mutable.Map.empty[String, VirtualJSFile]

    def addCoreJSLibFile(file: VirtualJSFile): Unit = {
      if (coreJSLibFile.nonEmpty)
        throw new IllegalStateException("Duplicate core JS lib on the classpath")
      coreJSLibFile = Some(file)
    }

    def addCoreInfoFile(file: VirtualFile): Unit = {
      coreInfoFiles += file
    }

    def hasClassFile(relativePathOfJSFile: String): Boolean =
      classFiles.contains(relativePathOfJSFile)

    def addClassFile(relativePathOfJSFile: String,
        file: VirtualScalaJSClassfile): Unit =
      classFiles += ((relativePathOfJSFile, file))

    def addClassFileIfNew(relativePathOfJSFile: String,
        file: => VirtualScalaJSClassfile): Boolean = {
      if (hasClassFile(relativePathOfJSFile)) false
      else {
        addClassFile(relativePathOfJSFile, file)
        true
      }
    }

    def hasJSFile(relativePath: String): Boolean =
      otherJSFiles.contains(relativePath)

    def addJSFile(relativePath: String, file: VirtualJSFile): Unit =
      otherJSFiles += ((relativePath, file))

    def addJSFileIfNew(relativePath: String, file: => VirtualJSFile): Boolean = {
      if (hasJSFile(relativePath)) false
      else {
        addJSFile(relativePath, file)
        true
      }
    }

    /** Returns the result of the builder.
     *  A core JS lib must have been found.
     */
    def result: ScalaJSClasspathEntries = {
      if (coreJSLibFile.isEmpty)
        throw new IllegalStateException("Missing core JS lib on the classpath")
      ScalaJSClasspathEntries(
          coreJSLibFile.get, coreInfoFiles.toSeq, classFiles.values.toSeq,
          otherJSFiles.values.toSeq)
    }

    /** Returns a partial result of the builder.
     *  There may or may not be a core JS lib (in which case it is an empty
     *  file).
     */
    def partialResult: ScalaJSClasspathEntries = {
      val coreJSLib = coreJSLibFile.getOrElse(
          VirtualJSFile.empty("scalajs-corejslib.js"))
      ScalaJSClasspathEntries(
          coreJSLib, coreInfoFiles.toSeq, classFiles.values.toSeq,
          otherJSFiles.values.toSeq)
    }
  }

  /** Reads and builds the Scala.js classpath entries in a File-based classpath. */
  def readEntriesInClasspath(classpath: Seq[File]): ScalaJSClasspathEntries = {
    readEntriesInClasspathInternal(classpath).result
  }

  /** Reads and builds the Scala.js classpath entries in a File-based classpath. */
  def readEntriesInClasspathPartial(classpath: Seq[File]): ScalaJSClasspathEntries = {
    readEntriesInClasspathInternal(classpath).partialResult
  }

  /** Reads and builds the Scala.js classpath entries in a File-based classpath. */
  private def readEntriesInClasspathInternal(classpath: Seq[File]): Builder = {
    val builder = new Builder
    for (element <- classpath)
      readEntriesInClasspathElement(builder, element)
    builder
  }

  /** Adds the Scala.js classpath entries in a directory or jar to a builder. */
  def readEntriesInClasspathElement(builder: Builder, element: File): Unit = {
    if (element.isDirectory)
      readEntriesInDir(builder, element)
    else if (element.isFile)
      readEntriesInJar(builder, element)
  }

  /** Adds the Scala.js classpath entries in a directory to a builder. */
  def readEntriesInDir(builder: Builder, dir: File): Unit = {
    def recurse(dir: File, dirPath: String): Unit = {
      val files = dir.listFiles
      for (file <- files) {
        val name = file.getName
        if (file.isDirectory) {
          recurse(file, dirPath + name + "/")
        } else {
          val path = dirPath + name
          path match {
            case "scalajs-corejslib.js" =>
              builder.addCoreJSLibFile(FileVirtualJSFile(file))

            case "javalangObject.sjsinfo" | "javalangString.sjsinfo" =>
              builder.addCoreInfoFile(FileVirtualJSFile(file))

            case _ if name.endsWith(".js") =>
              if (FileVirtualScalaJSClassfile.isScalaJSClassfile(file)) {
                builder.addClassFileIfNew(path,
                    FileVirtualScalaJSClassfile(file))
              } else {
                builder.addJSFileIfNew(path, FileVirtualJSFile(file))
              }
            case _ => // ignore other files
          }
        }
      }
    }
    recurse(dir, "")
  }

  /** Adds the Scala.js class files in a .jar file (or .zip) to a builder. */
  def readEntriesInJar(builder: Builder, jarFile: File): Unit = {
    val stream = new FileInputStream(jarFile)
    try {
      readEntriesInJar(builder, stream)
    } finally {
      stream.close()
    }
  }

  /** Adds the Scala.js class files in a .jar file (or .zip) to a builder. */
  def readEntriesInJar(builder: Builder, stream: InputStream): Unit = {
    val zipStream = new ZipInputStream(stream)
    val classFiles = mutable.Map.empty[String, MemVirtualScalaJSClassfile]

    def getOrCreateClassfile(path: String): MemVirtualScalaJSClassfile = {
      classFiles.getOrElseUpdate(path, {
        val name = path.substring(path.lastIndexOf('/')+1)
        new MemVirtualScalaJSClassfile(name)
      })
    }

    @tailrec
    def loop(): Unit = {
      val entry = zipStream.getNextEntry()
      if (entry != null) {
        val longName = entry.getName
        val name = longName.substring(longName.lastIndexOf('/')+1)

        def jsPath(ext: String) = longName.dropRight(ext.length) + ".js"

        def entryContent: String =
          IO.readInputStreamToString(zipStream)
        def entryVersion: Option[Any] =
          Some(entry.getTime)

        longName match {
          case "scalajs-corejslib.js" =>
            builder.addCoreJSLibFile(
                new MemVirtualJSFile(name)
                  .withContent(entryContent)
                  .withVersion(entryVersion))

          case "javalangObject.sjsinfo" | "javalangString.sjsinfo" =>
            builder.addCoreInfoFile(
                new MemVirtualFile(name)
                  .withContent(entryContent)
                  .withVersion(entryVersion))

          case _ =>
            if (name.endsWith(".js")) {
              // assume Scala.js class file
              val path = longName
              if (!builder.hasClassFile(path)) {
                getOrCreateClassfile(path)
                  .withContent(entryContent)
                  .withVersion(entryVersion)
              }
            } else if (name.endsWith(".js.map")) {
              // assume the source map of a Scala.js class file
              val path = jsPath(".js.map")
              if (!builder.hasClassFile(path)) {
                getOrCreateClassfile(path)
                  .withSourceMap(Some(entryContent))
              }
            } else if (name.endsWith(".sjsinfo")) {
              // the info of a Scala.js class file
              val path = jsPath(".sjsinfo")
              if (!builder.hasClassFile(path)) {
                getOrCreateClassfile(path)
                  .withInfo(entryContent)
              }
            } else {
              // ignore other files
            }
        }
        loop()
      }
    }
    loop()

    for ((path, classFile) <- classFiles) {
      if (classFile.info != "") // it is really a Scala.js class file
        builder.addClassFile(path, classFile)
      else // it is another .js file
        builder.addJSFile(path, classFile)
    }
  }
}
