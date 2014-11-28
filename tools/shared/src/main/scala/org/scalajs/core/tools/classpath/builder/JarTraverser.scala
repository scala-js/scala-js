/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.classpath.builder

import scala.collection.mutable
import scala.annotation.tailrec

import java.util.zip._
import java.io.{InputStream, InputStreamReader, Reader}

import org.scalajs.core.tools.io._
import org.scalajs.core.tools.jsdep.JSDependencyManifest

trait JarTraverser extends ClasspathContentHandler with FileSystem {

  private val jsFiles = mutable.Map.empty[String, MemVirtualJSFile]

  /** Traverse a Jar and return a version */
  protected def traverseJar(jar: File): String = {
    val zipStream = new ZipInputStream(toInputStream(jar))

    try readEntries(zipStream, getAbsolutePath(jar))
    finally zipStream.close()

    for {
      (_, jsFile) <- jsFiles
      if jsFile.content != "" // drop if this is just a lone sourcemap
    } handleJS(jsFile)

    getGlobalVersion(jar)
  }

  private def getOrCreateJSFile(relPath: String, fullPath: String, fn: String) =
    jsFiles.getOrElseUpdate(relPath, new MemVirtualJSFile(fullPath) {
      override val name = fn
    })

  @tailrec
  private def readEntries(in: ZipInputStream, jarPath: String): Unit = {
    val entry = in.getNextEntry()
    if (entry != null) {
      readEntry(entry, in, jarPath)
      readEntries(in, jarPath)
    }
  }

  private def readEntry(entry: ZipEntry, in: InputStream, jarPath: String) = {
    val longName = entry.getName
    val shortName = VirtualFile.nameFromPath(longName)
    val fullPath = jarPath + "#" + longName

    def entryReader: Reader = new InputStreamReader(in, "UTF-8")
    def entryContent: String = IO.readInputStreamToString(in)
    def entryBinaryContent: Array[Byte] = IO.readInputStreamToByteArray(in)
    def entryVersion: Option[String] = Some(entry.getTime.toString)

    if (longName == JSDependencyManifest.ManifestFileName)
      handleDepManifest(JSDependencyManifest.read(entryReader))
    else if (longName.endsWith(".js")) {
      val relPath = longName
      getOrCreateJSFile(relPath, fullPath, shortName)
        .withContent(entryContent)
        .withVersion(entryVersion)
    } else if (longName.endsWith(".js.map")) {
      // assume the source map of a JS file
      val relPath = longName.dropRight(".map".length)
      getOrCreateJSFile(relPath, fullPath, shortName)
        .withSourceMap(Some(entryContent))
    } else if (longName.endsWith(".sjsir")) {
      val vf = new MemVirtualSerializedScalaJSIRFile(fullPath) {
        override val name = shortName
      }.withContent(entryBinaryContent)
       .withVersion(entryVersion)

      handleIR(longName, vf)
    }
  }
}
