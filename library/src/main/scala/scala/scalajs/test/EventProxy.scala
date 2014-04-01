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
  def error(message: String, stack: js.Array[js.Any]): Unit = ???
  def failure(message: String, stack: js.Array[js.Any]): Unit = ???
  def succeeded(message: String): Unit = ???
  def skipped(message: String): Unit = ???
  def pending(message: String): Unit = ???
  def ignored(message: String): Unit = ???
  def canceled(message: String): Unit = ???

  def error(message: String): Unit = ???
  def info(message: String): Unit = ???
}
