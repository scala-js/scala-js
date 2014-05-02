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

import scala.util.Try

final class ScalaJSPackedClasspath private (
    val mainJSFiles: Seq[VirtualJSFile],
    val jsDependencies: Seq[VirtualJSFile]
) extends JSClasspath

object ScalaJSPackedClasspath {

  private val packOrderPrefix = "// ScalaJS PackOrder: "

  def packOrderLine(order: Int) = s"$packOrderPrefix$order"

  private def readPackOrder(f: VirtualJSFile): Int = {
    val in = new BufferedReader(f.reader)

    val optPackOrder = for {
      nullLine <- Try(in.readLine()).toOption
      line     <- Option(nullLine)
      if line.startsWith(packOrderPrefix)
      orderStr  = line.stripPrefix(packOrderPrefix)
      order    <- Try(orderStr.toInt).toOption
    } yield order

    in.close()

    optPackOrder.getOrElse(
      sys.error(s"$f doesn't have a ScalaJS PackOrder header"))
  }

  def apply(unorderedPackfiles: Seq[VirtualJSFile],
      jsDependencies: Seq[VirtualJSFile]): ScalaJSPackedClasspath = {
    val orderedPackfiles = unorderedPackfiles.sortBy(readPackOrder _)
    new ScalaJSPackedClasspath(orderedPackfiles, jsDependencies)
  }

}
