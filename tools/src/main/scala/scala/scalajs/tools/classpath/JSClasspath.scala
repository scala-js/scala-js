package scala.scalajs.tools.classpath

import scala.scalajs.tools.io.VirtualJSFile

import java.io._

trait JSClasspath {
  /** files that are loaded right away */
  def mainJSFiles: Seq[VirtualJSFile]
  /** files made available through a load mechanism */
  def otherJSFiles: Seq[VirtualJSFile]
}

object JSClasspath {
  def fromClasspath(classpath: Seq[File]): JSClasspath = {
    val builder = new ClasspathBuilder
    builder.readEntriesInClasspath(classpath)

    if (builder.isScalaJSClasspath)
      builder.result
    else if (builder.isScalaJSPackedClasspath)
      builder.packedResult
    else
      sys.error("Couldn't construct a concrete classpath from classpath")
  }
}
