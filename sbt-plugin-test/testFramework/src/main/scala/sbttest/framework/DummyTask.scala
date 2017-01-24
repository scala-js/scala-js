package sbttest.framework

import sbt.testing._

import org.scalajs.testinterface.TestUtils

final class DummyTask(
    val taskDef: TaskDef,
    runner: BaseRunner
) extends Task {

  def tags: Array[String] = Array()

  def execute(eventHandler: EventHandler, loggers: Array[Logger]): Array[Task] = {
    try {
      // Just create a new instance.
      val inst = TestUtils.newInstance(taskDef.fullyQualifiedName,
          runner.testClassLoader, Seq())(Seq())

      eventHandler.handle(new DummyEvent(taskDef, None))
      loggers.foreach(_.info(s"Success: ${taskDef.fullyQualifiedName}"))
    } catch {
      case t: Throwable =>
        eventHandler.handle(new DummyEvent(taskDef, Some(t)))
        loggers foreach { l =>
          l.error(s"Failed: ${taskDef.fullyQualifiedName}")
          l.trace(t)
        }
    }

    runner.taskDone()
    Array()
  }

  def execute(eventHandler: EventHandler, loggers: Array[Logger],
      continuation: Array[Task] => Unit): Unit = {
    continuation(execute(eventHandler, loggers))
  }

  private class DummyEvent(taskDef: TaskDef, t: Option[Throwable]) extends Event {
    val fullyQualifiedName: String = taskDef.fullyQualifiedName
    val fingerprint: Fingerprint = taskDef.fingerprint
    val selector: Selector = new SuiteSelector

    val status: Status =
      if (t.isDefined) Status.Error else Status.Success

    val throwable: OptionalThrowable =
      t.fold(new OptionalThrowable)(new OptionalThrowable(_))

    val duration: Long = -1L
  }

}
