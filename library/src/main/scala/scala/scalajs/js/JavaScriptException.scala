/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */



package scala.scalajs.js

case class JavaScriptException(exception: scala.Any) extends RuntimeException {
  override def getMessage(): String = exception.toString()

  override def fillInStackTrace(): Throwable = {
    scala.scalajs.runtime.StackTrace.captureState(this, exception)
    this
  }
}
