/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.testadapter

import sbt.testing._

import org.scalajs.testadapter.json._

@deprecated("Unused. Is going to be removed.", "0.6.20.")
final class RemoteException private (msg: String, _toString: String,
    cause: Throwable, val originalClass: String) extends Exception(msg, cause) {
  override def toString(): String = _toString
}

@deprecated("Unused. Is going to be removed.", "0.6.20.")
private[testadapter] object RemoteException {
  implicit object StackTraceDeserializer extends JSONDeserializer[StackTraceElement] {
    def deserialize(x: JSON): StackTraceElement = {
      val obj = new JSONObjExtractor(x)
      new StackTraceElement(
          obj.fld[String]("className"),
          obj.fld[String]("methodName"),
          obj.fld[String]("fileName"),
          obj.fld[Int]("lineNumber"))
    }
  }

  implicit object Deserializer extends JSONDeserializer[RemoteException] {
    def deserialize(x: JSON): RemoteException = {
      val obj = new JSONObjExtractor(x)

      val e = new RemoteException(
          obj.fld[String]("message"),
          obj.fld[String]("toString"),
          obj.opt[RemoteException]("cause").orNull,
          obj.fld[String]("class"))

      e.setStackTrace(obj.fld[List[StackTraceElement]]("stackTrace").toArray)

      e
    }
  }
}
