/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.testadapter

import sbt.testing._

import org.scalajs.core.tools.json._

import FingerprintSerializers._

private[testadapter] final class FrameworkInfo private (
    val name: String, val fingerprints: List[Fingerprint])

private[testadapter] object FrameworkInfo {
  implicit object Deserializer extends JSONDeserializer[FrameworkInfo] {
    def deserialize(x: JSON): FrameworkInfo = {
      val obj = new JSONObjExtractor(x)
      new FrameworkInfo(
          obj.fld[String]           ("name"),
          obj.fld[List[Fingerprint]]("fingerprints"))
    }
  }
}
