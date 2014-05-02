package scala.scalajs.tools.classpath

import scala.scalajs.tools.io.VirtualJSFile

import java.io._

trait JSClasspath {
  def mainJSFiles: Seq[VirtualJSFile]
  def jsDependencies: Seq[VirtualJSFile]
  /** all JS files in the classpath */
  final def jsFiles: Seq[VirtualJSFile] = jsDependencies ++ mainJSFiles
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
