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

package org.scalajs.junit

import sbt.testing._

private[junit] final class JUnitEvent(
    taskDef: TaskDef,
    val status: Status,
    val selector: Selector,
    val throwable: OptionalThrowable = new OptionalThrowable,
    val duration: Long = -1L
) extends Event {
  def fullyQualifiedName: String = taskDef.fullyQualifiedName
  def fingerprint: Fingerprint = taskDef.fingerprint
}
