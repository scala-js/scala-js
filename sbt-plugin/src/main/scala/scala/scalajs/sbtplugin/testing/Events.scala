/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.sbtplugin.testing

import sbt.testing.{Event => SbtEvent, _}

class Events(taskDef: TaskDef) {

  abstract class Event(val status: Status,
      val throwable: OptionalThrowable = new OptionalThrowable) extends SbtEvent {
    val fullyQualifiedName = taskDef.fullyQualifiedName
    val fingerprint = taskDef.fingerprint
    val selector = taskDef.selectors.headOption.getOrElse(new SuiteSelector)
    val duration = -1L
  }

  case class Error(exception: Throwable) extends Event(
      Status.Error, new OptionalThrowable(exception))

  case class Failure(exception: Throwable) extends Event(
      Status.Failure, new OptionalThrowable(exception))

  case object Succeeded extends Event(Status.Success)
  case object Skipped extends Event(Status.Skipped)
  case object Pending extends Event(Status.Pending)
  case object Ignored extends Event(Status.Ignored)
  case object Canceled extends Event(Status.Canceled)
}
