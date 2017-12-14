/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */



package scala.scalajs.js

import scala.language.reflectiveCalls

import scala.scalajs.js

final case class JavaScriptException(exception: scala.Any)
    extends RuntimeException {

  override def getMessage(): String = exception.toString()

  override def fillInStackTrace(): Throwable = {
    type JSExceptionEx = JavaScriptException {
      def setStackTraceStateInternal(e: scala.Any): Unit
    }
    this.asInstanceOf[JSExceptionEx].setStackTraceStateInternal(exception)
    this
  }
}
