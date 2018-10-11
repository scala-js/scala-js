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

/** Binary compat only. */
@deprecated("Unused, use TestAdapter instead", "0.6.22")
final class ScalaJSRunner private () extends Runner {
  def args(): Array[String] = ???
  def done(): String = ???
  def remoteArgs(): Array[String] = ???
  def tasks(x: Array[TaskDef]): Array[Task] = ???
}
