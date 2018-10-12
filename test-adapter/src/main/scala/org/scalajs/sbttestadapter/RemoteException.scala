/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.testadapter

import sbt.testing._

import org.scalajs.core.tools.json._

@deprecated("Unused. Is going to be removed.", "0.6.20.")
final class RemoteException private (msg: String, _toString: String,
    cause: Throwable, val originalClass: String) extends Exception(msg, cause) {
  override def toString(): String = _toString
}

@deprecated("Unused. Is going to be removed.", "0.6.20.")
object RemoteException {
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
