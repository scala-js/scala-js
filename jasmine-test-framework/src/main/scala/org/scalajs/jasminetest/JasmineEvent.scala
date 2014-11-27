/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Framework    **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.jasminetest

import sbt.testing._

final class JasmineEvent(
    taskDef: TaskDef,
    val status: Status,
    val selector: Selector,
    val throwable: OptionalThrowable = new OptionalThrowable,
    val duration: Long = -1L
) extends Event {
  def fullyQualifiedName: String = taskDef.fullyQualifiedName
  def fingerprint: Fingerprint = taskDef.fingerprint
}
