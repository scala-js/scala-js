package scala.scalajs.tools.classpath

import scala.scalajs.tools.io.VirtualJSFile

trait JSClasspath {
  /** files that are loaded right away */
  def mainJSFiles: Seq[VirtualJSFile]
  /** files made available through a load mechanism */
  def otherJSFiles: Seq[VirtualJSFile]
}
