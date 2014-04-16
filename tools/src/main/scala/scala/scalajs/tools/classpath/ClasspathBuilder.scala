package scala.scalajs.tools.classpath

import scala.scalajs.tools.io._

import java.io._
import java.util.zip._

import scala.collection.mutable

import scala.annotation.tailrec

private[classpath] class ClasspathBuilder {

  private var coreJSLibFile: Option[VirtualJSFile] = None
  private val coreInfoFiles = mutable.ListBuffer.empty[VirtualFile]
  private val classFiles = mutable.Map.empty[String, VirtualScalaJSClassfile]
  private val packFiles = mutable.Map.empty[String, VirtualScalaJSPackfile]
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

  def hasPackFile(relativePathOfJSFile: String): Boolean =
    packFiles.contains(relativePathOfJSFile)

  def addPackFile(relativePathOfJSFile: String,
      file: VirtualScalaJSPackfile): Unit =
    packFiles += ((relativePathOfJSFile, file))

  def addPackFileIfNew(relativePathOfJSFile: String,
      file: => VirtualScalaJSPackfile): Boolean = {
    if (hasPackFile(relativePathOfJSFile)) false
    else {
      addPackFile(relativePathOfJSFile, file)
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

  def isScalaJSClasspath = coreJSLibFile.isDefined && packFiles.isEmpty
  def isPartialScalaJSClasspath = packFiles.isEmpty
  def isScalaJSPackedClasspath = packFiles.nonEmpty

  /** Returns the result of the builder.
   *  A core JS lib must have been found.
   */
  def result: ScalaJSClasspath = {
    if (!isScalaJSClasspath)
      throw new IllegalStateException("Missing core JS lib on the classpath")
    ScalaJSClasspath(
        coreJSLibFile.get, coreInfoFiles.toSeq, classFiles.values.toSeq,
        otherJSFiles.values.toSeq)
  }

  /** Returns a partial result of the builder.
   *  There may or may not be a core JS lib (in which case it is an empty
   *  file).
   */
  def partialResult: ScalaJSClasspath = {
    if (!isPartialScalaJSClasspath)
      throw new IllegalStateException("Pack files on classpath")
    val coreJSLib = coreJSLibFile.getOrElse(
        VirtualJSFile.empty("scalajs-corejslib.js"))
    ScalaJSClasspath(
        coreJSLib, coreInfoFiles.toSeq, classFiles.values.toSeq,
        otherJSFiles.values.toSeq)
  }

  /** Returns a packed classpath. There may only be packFiles and
   *  otherJSFiles */
  def packedResult: ScalaJSPackedClasspath = {
    if (!isScalaJSPackedClasspath)
      throw new IllegalStateException("Packed classpaths may only contain packed files and others")

    // Note that we ignore a potential coreJSLib, coreInfoFile and classFiles
    ScalaJSPackedClasspath(packFiles.values.toSeq, otherJSFiles.values.toSeq)
  }

  /** Reads the Scala.js classpath entries in a File-based classpath. */
  def readEntriesInClasspath(classpath: Seq[File]): Unit = {
    for (element <- classpath)
      readEntriesInClasspathElement(element)
  }

  /** Adds the Scala.js classpath entries in a directory or jar to a builder. */
  def readEntriesInClasspathElement(element: File): Unit = {
    if (element.isDirectory)
      readEntriesInDir(element)
    else if (element.isFile) {
      if (FileVirtualScalaJSPackfile.isScalaJSPackfile(element))
        // A classpath entry may be a packfile
        // Note that this is NOT a valid location for a scalajs-corejslib.js
        addPackFileIfNew(element.getName, FileVirtualScalaJSPackfile(element))
      else
        // We assume it is a jar
        readEntriesInJar(element)
    }
  }

  def readJSFile(file: File, path: String) = {
    if (FileVirtualScalaJSClassfile.isScalaJSClassfile(file))
      addClassFileIfNew(path, FileVirtualScalaJSClassfile(file))
    else
      addJSFileIfNew(path, FileVirtualJSFile(file))
  }

  /** Adds the Scala.js classpath entries in a directory to a builder. */
  def readEntriesInDir(dir: File): Unit = {
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
              addCoreJSLibFile(FileVirtualJSFile(file))

            case "javalangObject.sjsinfo" | "javalangString.sjsinfo" =>
              addCoreInfoFile(FileVirtualJSFile(file))

            case _ if name.endsWith(".js") =>
              readJSFile(file, path)

            case _ => // ignore other files
          }
        }
      }
    }
    recurse(dir, "")
  }

  /** Adds the Scala.js class files in a .jar file (or .zip) to a builder. */
  def readEntriesInJar(jarFile: File): Unit = {
    val stream = new FileInputStream(jarFile)
    try {
      readEntriesInJar(stream, jarFile.getPath)
    } finally {
      stream.close()
    }
  }

  /** Adds the Scala.js class files in a .jar file (or .zip) to a builder.
   *  Note: This currently assumes that packfiles are not in jars
   */
  def readEntriesInJar(stream: InputStream,
      jarPath: String): Unit = {
    val zipStream = new ZipInputStream(stream)
    val classFiles = mutable.Map.empty[String, MemVirtualScalaJSClassfile]
    var coreJSLib: Option[MemVirtualJSFile] = None

    def getOrCreateClassfile(path: String): MemVirtualScalaJSClassfile =
      classFiles.getOrElseUpdate(path, new MemVirtualScalaJSClassfile(path))

    def getOrCreateCorejslib(jarPath: String): MemVirtualJSFile = {
      coreJSLib.getOrElse {
        val file = new MemVirtualJSFile(jarPath + ":scalajs-corejslib.js")
        coreJSLib = Some(file)
        file
      }
    }

    @tailrec
    def loop(): Unit = {
      val entry = zipStream.getNextEntry()
      if (entry != null) {
        val longName = entry.getName
        val name = longName.substring(longName.lastIndexOf('/')+1)
        val fullPath = jarPath + ":" + longName

        def jsPath(ext: String) = fullPath.dropRight(ext.length) + ".js"

        def entryContent: String =
          IO.readInputStreamToString(zipStream)
        def entryVersion: Option[Any] =
          Some(entry.getTime)

        longName match {
          case "scalajs-corejslib.js" =>
            getOrCreateCorejslib(jarPath)
              .withContent(entryContent)
              .withVersion(entryVersion)

          case "scalajs-corejslib.js.map" =>
            getOrCreateCorejslib(jarPath)
              .withSourceMap(Some(entryContent))

          case "javalangObject.sjsinfo" | "javalangString.sjsinfo" =>
            addCoreInfoFile(
                new MemVirtualFile(fullPath)
                  .withContent(entryContent)
                  .withVersion(entryVersion))

          case _ =>
            if (name.endsWith(".js")) {
              // assume Scala.js class file
              val path = fullPath
              if (!hasClassFile(path)) {
                getOrCreateClassfile(path)
                  .withContent(entryContent)
                  .withVersion(entryVersion)
              }
            } else if (name.endsWith(".js.map")) {
              // assume the source map of a Scala.js class file
              val path = jsPath(".js.map")
              if (!hasClassFile(path)) {
                getOrCreateClassfile(path)
                  .withSourceMap(Some(entryContent))
              }
            } else if (name.endsWith(".sjsinfo")) {
              // the info of a Scala.js class file
              val path = jsPath(".sjsinfo")
              if (!hasClassFile(path)) {
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

    coreJSLib.foreach(addCoreJSLibFile _)

    for ((path, classFile) <- classFiles) {
      if (classFile.info != "") // it is really a Scala.js class file
        addClassFile(path, classFile)
      else // it is another .js file
        addJSFile(path, classFile)
    }
  }

}
