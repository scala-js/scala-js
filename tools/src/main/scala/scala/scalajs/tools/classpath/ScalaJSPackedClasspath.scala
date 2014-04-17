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
import scala.scalajs.tools.json._

import org.json.simple.JSONValue

final class ScalaJSPackedClasspath private (
    val mainJSFiles: Seq[VirtualScalaJSPackfile],
    val otherJSFiles: Seq[VirtualJSFile]
) extends JSClasspath

object ScalaJSPackedClasspath {

  case class PackInfoData(packOrder: Int)

  object PackInfoData {

    implicit object packInfoDataToJSON extends JSONSerializer[PackInfoData] {
      def serialize(x: PackInfoData) = {
        new JSONObjBuilder()
          .fld("packOrder", x.packOrder)
          .toJSON
      }
    }

    implicit object packInfoDataFromJSON extends JSONDeserializer[PackInfoData] {
      def deserialize(x: Object): PackInfoData = {
        val e = new JSONObjExtractor(x)
        PackInfoData(e.fld[Int]("packOrder"))
      }
    }
  }

  def readPackInfo(file: VirtualScalaJSPackfile): PackInfoData =
    fromJSON[PackInfoData](JSONValue.parseWithException(file.packInfo))

  def writePackInfo(writer: VirtualScalaJSPackfileWriter,
      info: PackInfoData): Unit = {
    JSONValue.writeJSONString(info.toJSON, writer.packInfoWriter)
  }

  def apply(unorderedPackfiles: Seq[VirtualScalaJSPackfile],
      otherJSFiles: Seq[VirtualJSFile]): ScalaJSPackedClasspath = {

    val orderedPackfiles = unorderedPackfiles.sortBy { f =>
      readPackInfo(f).packOrder
    }

    new ScalaJSPackedClasspath(orderedPackfiles, otherJSFiles)
  }

}
