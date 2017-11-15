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
final class ScalaJSRunner private () extends Runner {
  def args(): Array[String] = ???
  def done(): String = ???
  def remoteArgs(): Array[String] = ???
  def tasks(x: Array[TaskDef]): Array[Task] = ???
}
