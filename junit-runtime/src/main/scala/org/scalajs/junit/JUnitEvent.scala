package org.scalajs.junit

import sbt.testing._

final class JUnitEvent(taskDef: TaskDef, val status: Status, val selector: Selector,
    val throwable: OptionalThrowable = new OptionalThrowable,
    val duration: Long = -1L) extends Event {
  def fullyQualifiedName: String = taskDef.fullyQualifiedName
  def fingerprint: Fingerprint = taskDef.fingerprint
}
