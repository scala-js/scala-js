package scala.scalajs.tools.classpath

import scala.scalajs.tools.io._

import java.io._
import java.util.zip._

import scala.collection.mutable

import scala.annotation.tailrec

private[classpath] class ClasspathBuilder {

  private var coreJSLibFile: Option[VirtualJSFile] = None
  private val irFiles = mutable.Map.empty[String, VirtualScalaJSIRFile]
  private val packFiles = mutable.Map.empty[String, VirtualJSFile]
  private val otherJSFiles = mutable.Map.empty[String, VirtualJSFile]

  def addCoreJSLibFile(file: VirtualJSFile): Unit = {
    if (coreJSLibFile.nonEmpty)
      throw new IllegalStateException("Duplicate core JS lib on the classpath")
    coreJSLibFile = Some(file)
  }

  def hasIRFile(relativePathOfIRFile: String): Boolean =
    irFiles.contains(relativePathOfIRFile)

  def addIRFile(relativePathOfIRFile: String,
      file: VirtualScalaJSIRFile): Unit =
    irFiles += ((relativePathOfIRFile, file))

  def addIRFileIfNew(relativePathOfIRFile: String,
      file: => VirtualScalaJSIRFile): Boolean = {
    if (hasIRFile(relativePathOfIRFile)) false
    else {
      addIRFile(relativePathOfIRFile, file)
      true
    }
  }

  def hasPackFile(relativePathOfJSFile: String): Boolean =
    packFiles.contains(relativePathOfJSFile)

  def addPackFile(relativePathOfJSFile: String,
      file: VirtualJSFile): Unit =
    packFiles += ((relativePathOfJSFile, file))

  def addPackFileIfNew(relativePathOfJSFile: String,
      file: => VirtualJSFile): Boolean = {
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
        coreJSLibFile.get, irFiles.values.toSeq, otherJSFiles.values.toSeq)
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
        coreJSLib, irFiles.values.toSeq, otherJSFiles.values.toSeq)
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
      val name = element.getName
      if (name.endsWith(".js"))
        // A classpath entry which is a js file must a packfile
        // Note that this is NOT a valid location for any other js file
        addPackFileIfNew(name, FileVirtualJSFile(element))
      else
        // We assume it is a jar
        readEntriesInJar(element)
    }
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

            case _ if name.endsWith(".js") =>
              addJSFileIfNew(path, FileVirtualJSFile(file))

            case _ if name.endsWith(".sjsir") =>
              addIRFileIfNew(path, FileVirtualScalaJSIRFile(file))

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
    val jsFiles = mutable.Map.empty[String, MemVirtualJSFile]
    var coreJSLib: Option[MemVirtualJSFile] = None

    def getOrCreateJSFile(relPath: String, fullPath: String): MemVirtualJSFile =
      jsFiles.getOrElseUpdate(relPath, new MemVirtualJSFile(fullPath))

    def getOrCreateCorejslib(fullPath: String): MemVirtualJSFile = {
      coreJSLib.getOrElse {
        val file = new MemVirtualJSFile(fullPath)
        coreJSLib = Some(file)
        file
      }
    }

    @tailrec
    def loop(): Unit = {
      val entry = zipStream.getNextEntry()
      if (entry != null) {
        val longName = entry.getName
        val fullPath = jarPath + ":" + longName

        def entryContent: String =
          IO.readInputStreamToString(zipStream)
        def entryBinaryContent: Array[Byte] =
          IO.readInputStreamToByteArray(zipStream)
        def entryVersion: Option[Any] =
          Some(entry.getTime)

        longName match {
          case "scalajs-corejslib.js" =>
            getOrCreateCorejslib(fullPath)
              .withContent(entryContent)
              .withVersion(entryVersion)

          case "scalajs-corejslib.js.map" =>
            getOrCreateCorejslib(jarPath)
              .withSourceMap(Some(entryContent))

          case _ =>
            if (longName.endsWith(".js")) {
              // content of a JS file
              val relPath = longName
              if (!hasJSFile(relPath)) {
                getOrCreateJSFile(relPath, fullPath)
                  .withContent(entryContent)
                  .withVersion(entryVersion)
              }
            } else if (longName.endsWith(".js.map")) {
              // assume the source map of a JS file
              val relPath = longName.dropRight(".map".length)
              if (!hasJSFile(relPath)) {
                getOrCreateJSFile(relPath, fullPath)
                  .withSourceMap(Some(entryContent))
              }
            } else if (longName.endsWith(".sjsir")) {
              // a Scala.js IR file
              val relPath = longName
              if (!hasIRFile(relPath)) {
                addIRFile(relPath,
                    new MemVirtualSerializedScalaJSIRFile(fullPath)
                      .withContent(entryBinaryContent)
                      .withVersion(entryVersion))
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

    for ((path, jsFile) <- jsFiles) {
      if (jsFile.content != "") // it is not just an unpaired .js.map file
        addJSFile(path, jsFile)
    }
  }

}
