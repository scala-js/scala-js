/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.testadapter

import sbt.testing._

/** Binary compat only. */
@deprecated("Unused, use TestAdapter instead", "0.6.22")
final class ScalaJSTask private () extends Task {
  def execute(x: EventHandler,y: Array[Logger]): Array[Task] = ???
  def tags(): Array[String] = ???
  def taskDef(): TaskDef = ???
}

/** Binary compat only. */
@deprecated("Unused, use TestAdapter instead", "0.6.22")
object ScalaJSTask
