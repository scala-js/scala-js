package org.scalajs.testinterface.internal

import scala.scalajs.js
import js.Dynamic.{literal => lit}
import js.JSConverters._

import sbt.testing._

object ThrowableSerializer {

  def serialize(t: Throwable): js.Dynamic = {
    val res = lit(
        `class` = t.getClass().toString,
        message = t.getMessage(),
        toString = t.toString(),
        stackTrace = t.getStackTrace().map(serializeTraceElem).toJSArray)

    if (t.getCause() != null)
      res.cause = serialize(t.getCause())

    res
  }

  private def serializeTraceElem(e: StackTraceElement): js.Dynamic = {
    lit(className = e.getClassName,
        methodName = e.getMethodName,
        fileName = e.getFileName,
        lineNumber = e.getLineNumber)
  }

}
