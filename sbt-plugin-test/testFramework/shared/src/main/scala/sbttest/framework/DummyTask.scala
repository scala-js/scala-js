package sbttest.framework

import sbt.testing._

import scala.concurrent.ExecutionContext.Implicits.global

final class DummyTask(
    val taskDef: TaskDef,
    runner: BaseRunner
) extends Task {

  def tags: Array[String] = Array()

  def execute(eventHandler: EventHandler, loggers: Array[Logger]): Array[Task] = {
    try {
      // Just create a new instance of the test class, for its side effects.
      Platform.instantiateTestClass(taskDef.fullyQualifiedName,
          runner.testClassLoader)

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
    val tasks = execute(eventHandler, loggers)
    runner.taskBlock.foreach(_ => continuation(tasks))
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
