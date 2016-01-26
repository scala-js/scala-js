/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.io

import java.io.{File => JFile}

import org.scalajs.core.tools.io.IRFileCache.IRContainer

trait IRContainerPlatformExtensions { this: IRContainer.type =>
  def fromClasspath(classpath: Seq[JFile]): Seq[IRContainer] = {
    classpath flatMap { entry =>
      if (!entry.exists)
        Nil
      else if (entry.isDirectory)
        fromDirectory(entry)
      else if (entry.getName.endsWith(".jar"))
        fromJar(entry) :: Nil
      else
        throw new IllegalArgumentException("Illegal classpath entry " + entry)
    }
  }

  def fromJar(jar: JFile): Jar = {
    require(jar.isFile)
    val vf = new FileVirtualBinaryFile(jar) with VirtualJarFile
    Jar(vf)
  }

  def fromDirectory(dir: JFile): Seq[File] = {
    require(dir.isDirectory)

    val baseDir = dir.getAbsoluteFile

    def walkForIR(dir: JFile): Seq[JFile] = {
      val (subdirs, files) = dir.listFiles().partition(_.isDirectory)
      subdirs.flatMap(walkForIR) ++ files.filter(_.getName.endsWith(".sjsir"))
    }

    for (ir <- walkForIR(baseDir)) yield {
      val relDir = ir.getPath.stripPrefix(baseDir.getPath)
      val vf = FileVirtualScalaJSIRFile.relative(ir, relDir)
      File(vf)
    }
  }
}
