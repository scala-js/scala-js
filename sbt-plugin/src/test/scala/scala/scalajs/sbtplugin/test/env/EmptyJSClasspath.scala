package scala.scalajs.sbtplugin.test.env

import scala.scalajs.tools.classpath._
import scala.scalajs.tools.io._

object EmptyJSClasspath extends JSClasspath {
  def mainJSFiles: Seq[VirtualJSFile] = Seq.empty
  def otherJSFiles: Seq[VirtualJSFile] = Seq.empty
}
