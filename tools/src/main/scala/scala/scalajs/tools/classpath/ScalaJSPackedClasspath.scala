/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.tools.classpath

import java.io._
import scala.scalajs.tools.io._

import net.liftweb.json._

final class ScalaJSPackedClasspath private (
    val mainJSFiles: Seq[VirtualScalaJSPackfile],
    val otherJSFiles: Seq[VirtualJSFile]
) extends JSClasspath

object ScalaJSPackedClasspath {

  case class PackInfoData(packOrder: Int)

  def readPackInfo(file: VirtualScalaJSPackfile): PackInfoData = {
    implicit val formats = DefaultFormats
    Serialization.read[PackInfoData](file.packInfo)
  }

  def writePackInfo(writer: VirtualScalaJSPackfileWriter,
      info: PackInfoData): Unit = {
    implicit val formats = DefaultFormats
    writer.packInfoWriter.write(Serialization.write(info))
  }

  def apply(unorderedPackfiles: Seq[VirtualScalaJSPackfile],
      otherJSFiles: Seq[VirtualJSFile]): ScalaJSPackedClasspath = {

    val orderedPackfiles = unorderedPackfiles.sortBy { f =>
      readPackInfo(f).packOrder
    }

    new ScalaJSPackedClasspath(orderedPackfiles, otherJSFiles)
  }

}
