package scala.scalajs.tools.classpath

import scala.scalajs.tools.io._
import scala.scalajs.tools.jsdep._

import java.io._
import java.util.zip._

import scala.collection.mutable

import scala.annotation.tailrec

private[classpath] class ClasspathBuilder {

  private val irFiles = mutable.Map.empty[String, VirtualScalaJSIRFile]
  private val packFiles = mutable.Map.empty[String, VirtualJSFile]
  private val otherJSFiles = mutable.Map.empty[String, VirtualJSFile]
  private val jsDepManifests = mutable.ListBuffer.empty[JSDependencyManifest]

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

  def addDependencyManifestFile(file: VirtualTextFile): Unit = {
    jsDepManifests += JSDependencyManifest.read(file)
  }

  def isScalaJSClasspath = packFiles.isEmpty
  def isScalaJSPackedClasspath = packFiles.nonEmpty

  def jsDependencies = {
    val includeList = JSDependencyManifest.createIncludeList(jsDepManifests)
    for {
      name <- includeList
    } yield otherJSFiles.find(_._1.endsWith(name)).getOrElse(
        sys.error(s"$name is declared as JS dependency but not on classpath")
    )._2
  }

  /** Returns the result of the builder. There may not be any packFiles */
  def result: ScalaJSClasspath = {
    if (!isScalaJSClasspath)
      sys.error("Not an unpacked classpath (has packfiles)")
    ScalaJSClasspath(irFiles.values.toSeq, jsDependencies)
  }

  /** Returns a packed classpath. There must be packFiles */
  def packedResult: ScalaJSPackedClasspath = {
    if (!isScalaJSPackedClasspath)
      sys.error("Not a packed classpath (no packfile)")

    // Note that we ignore potential irFiles
    ScalaJSPackedClasspath(packFiles.values.toSeq, jsDependencies)
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
            case JSDependencyManifest.ManifestFileName =>
              addDependencyManifestFile(FileVirtualTextFile(file))

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

    def getOrCreateJSFile(relPath: String, fullPath: String): MemVirtualJSFile =
      jsFiles.getOrElseUpdate(relPath, new MemVirtualJSFile(fullPath))

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
          case JSDependencyManifest.ManifestFileName =>
              addDependencyManifestFile(
                  new MemVirtualTextFile(fullPath).withContent(entryContent))

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

    for ((path, jsFile) <- jsFiles) {
      if (jsFile.content != "") // it is not just an unpaired .js.map file
        addJSFile(path, jsFile)
    }
  }

}
