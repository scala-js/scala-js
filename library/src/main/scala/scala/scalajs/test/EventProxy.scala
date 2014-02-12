/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.test

import scala.scalajs.js

trait EventProxy extends js.Object {
  def error(message: js.String, stack: js.Array[js.Any]): Unit = ???
  def failure(message: js.String, stack: js.Array[js.Any]): Unit = ???
  def succeeded(message: js.String): Unit = ???
  def skipped(message: js.String): Unit = ???
  def pending(message: js.String): Unit = ???
  def ignored(message: js.String): Unit = ???
  def canceled(message: js.String): Unit = ???

  def error(message: js.String): Unit = ???
  def info(message: js.String): Unit = ???
}
