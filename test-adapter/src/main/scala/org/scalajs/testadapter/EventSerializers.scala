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
import SelectorSerializers._

import language.implicitConversions

private[testadapter] object EventSerializers {

  implicit object EventDeserializer extends JSONDeserializer[Event] {
    private implicit def optT2optT(x: Option[Throwable]): OptionalThrowable =
      x.fold(new OptionalThrowable)(t => new OptionalThrowable(t))

    def deserialize(x: JSON): Event = {
      val obj = new JSONObjExtractor(x)

      new DeserializedEvent(
          obj.fld[String]("fullyQualifiedName"),
          obj.fld[Fingerprint]("fingerprint"),
          obj.fld[Selector]("selector"),
          Status.valueOf(obj.fld[String]("status")),
          obj.opt[RemoteException]("throwable"),
          (obj.fld[Int]("durationMS").toLong << 32) |
          (obj.fld[Int]("durationLS").toLong & 0xffffffffL))
    }
  }

  final class DeserializedEvent(
      val fullyQualifiedName: String,
      val fingerprint: Fingerprint,
      val selector: Selector,
      val status: Status,
      val throwable: OptionalThrowable,
      val duration: Long
  ) extends Event

}
