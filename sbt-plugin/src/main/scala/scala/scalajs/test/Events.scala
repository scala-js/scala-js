package scala.scalajs.test

import sbt.testing.TaskDef
import sbt.testing.OptionalThrowable
import sbt.testing.{ Event => SbtEvent }
import sbt.testing.Status
import sbt.testing.SuiteSelector

class Events(taskDef: TaskDef) {

  abstract class Event(val status: Status, val throwable: OptionalThrowable = new OptionalThrowable) extends SbtEvent {
    val fullyQualifiedName = taskDef.fullyQualifiedName
    val fingerprint = taskDef.fingerprint
    val selector = taskDef.selectors.headOption.getOrElse(new SuiteSelector)
    val duration = -1L
  }

  case class Error(exception: Throwable) extends Event(Status.Error, new OptionalThrowable(exception))
  case class Failure(exception: Throwable) extends Event(Status.Failure, new OptionalThrowable(exception))
  case object Succeeded extends Event(Status.Success)
  case object Skipped extends Event(Status.Skipped)
  case object Pending extends Event(Status.Pending)
  case object Ignored extends Event(Status.Ignored)
  case object Canceled extends Event(Status.Canceled)
}
