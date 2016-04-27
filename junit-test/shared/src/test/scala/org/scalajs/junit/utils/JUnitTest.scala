package org.scalajs.junit.utils

import sbt.testing._
import org.junit.Test

import scala.annotation.tailrec

abstract class JUnitTest {
  import JUnitTest._

  type Output = JUnitTest.Output

  // ANSI colors codes
  private final val NORMAL = "\u001B[0m"
  private final val RED = "\u001B[31m"
  private final val YELLOW = "\u001B[33m"
  private final val BLUE = "\u001B[34m"
  private final val MAGENTA = "\u001B[35m"
  private final val CYAN = "\u001B[36m"
  private final val GREY = "\u001B[90m"

  private lazy val TIME_TAG = GREY + "<TIME>" + NORMAL

  // appropriate class loader for platform, needs platform extension
  private val classLoader: ClassLoader =
    JUnitTestPlatformImpl.getClassLoader

  private val suiteUnderTestName = {
    val myName = getClass.getName
    assert(myName.endsWith("Assertions"))
    myName.stripSuffix("Assertions")
  }

  protected def frameworkArgss: List[List[String]]

  protected def expectedOutput(context: OutputContext): List[Output]

  val expectedFail = 0
  val expectedIgnored = 0
  val expectedTotal = 0

  @Test def testJUnitOutput(): Unit = {
    for (frameworkArgs <- frameworkArgss) {
      val jUnitFramework = new com.novocode.junit.JUnitFramework()
      val runner =
        jUnitFramework.runner(frameworkArgs.toArray, Array.empty, classLoader)
      val tasks = runner.tasks(Array(new TaskDef(suiteUnderTestName,
        jUnitFramework.fingerprints.head, true, Array.empty)))

      val recorder: JUnitTestRecorder =
        new JUnitTestRecorder(new OutputContext(frameworkArgs))

      // run all tasks and the tasks they generate, needs platform extension
      JUnitTestPlatformImpl.executeLoop(tasks, recorder)

      recorder.recordDone(runner.done())

      recorder.checkOutput()
    }
  }

  protected class OutputContext(frameworkArgs: List[String]) {

    lazy val verbose: Boolean = frameworkArgs.contains("-v")
    lazy val noColor: Boolean = frameworkArgs.contains("-n")
    lazy val quiet: Boolean = frameworkArgs.contains("-q")
    lazy val decodeScalaNames: Boolean = frameworkArgs.contains("-s")
    lazy val notLogExceptionClass: Boolean = frameworkArgs.contains("-c")
    lazy val logAssert: Boolean = frameworkArgs.contains("-a")

    def formattedTestClass: String = {
      val (pack, cls) =
        suiteUnderTestName.splitAt(suiteUnderTestName.lastIndexOf('.'))
      pack + '.' + yellow(cls.tail)
    }

    def formattedAssumptionViolatedException: String =
      "org.junit.internal." + red("AssumptionViolatedException")

    def done: Output = Done("")

    def testStartedOutput(method: String): Output = {
      infoIfOrElseDebug(verbose,
          s"Test $formattedTestClass.${cyan(method)} started")
    }

    def testIgnoredOutput(method: String): Output =
      Info(s"Test $formattedTestClass.${cyan(method)} ignored")

    def testIgnoredClassOutput: Output =
      Info(s"Test $formattedTestClass ignored")

    def testExceptionMsgOutput(method: String, msg: String, exPack: String,
        exClass: String): Output = {
      val exClassStr =
        if (notLogExceptionClass) ""
        else ' ' + exPack + '.' + red(exClass) + ':'
      Error(s"Test $formattedTestClass.${red(method)} " +
          s"failed:$exClassStr $msg, took $TIME_TAG sec")
    }

    def testAssertionErrorMsgOutput(method: String, msg: String): Output = {
      val assertClass =
        if (notLogExceptionClass || !logAssert) ""
        else  " java.lang." + red("AssertionError") + ':'
      Error(s"Test $formattedTestClass.${red(method)} failed:$assertClass " +
          s"$msg, took $TIME_TAG sec")
    }

    def testAssumptionViolatedOutput(method: String): Output = {
      Warn(s"Test assumption in test $formattedTestClass.${red(method)}" +
          s" failed: $formattedAssumptionViolatedException:" +
          s" This assume should not pass, took $TIME_TAG sec")
    }

    def testFinishedOutput(method: String): Output = {
      Debug(s"Test $formattedTestClass.${cyan(method)} finished, " +
          s"took $TIME_TAG sec")
    }

    def testRunStartedOutput: Output =
      infoIfOrElseDebug(verbose, blue("Test run started"))

    def testRunFinishedOutput: Output = {
      val buff = new StringBuilder
      buff.append(blue("Test run finished: "))
      val failedColor = if (expectedFail == 0) blue _ else red _
      buff.append(failedColor(s"$expectedFail failed"))
      buff.append(blue(", "))
      val ignoredColor = if (expectedIgnored == 0) blue _ else yellow _
      buff.append(ignoredColor(s"$expectedIgnored ignored"))
      buff.append(blue(s", $expectedTotal total, ${TIME_TAG}s"))
      infoIfOrElseDebug(verbose, buff.result())
    }

    def skippedEvent: Output = Event("Skipped")
    def successEvent: Output = Event("Success")
    def failureEvent: Output = Event("Failure")

    def infoIfOrElseDebug(p: Boolean, msg: String): Output =
      if (p) Info(msg)
      else Debug(msg)

    override def toString: String = frameworkArgs.mkString("[", ", ", "]")

    private def red(str: String): String =
      if (noColor) str
      else RED + str + NORMAL

    private def yellow(str: String): String =
      if (noColor) str
      else YELLOW + str + NORMAL

    private def blue(str: String): String =
      if (noColor) str
      else BLUE + str + NORMAL

    private def magenta(str: String): String =
      if (noColor) str
      else MAGENTA + str + NORMAL

    private def cyan(str: String): String =
      if (noColor) str
      else CYAN + str + NORMAL
  }

  protected class JUnitTestRecorder(context: OutputContext)
      extends Logger with EventHandler {
    val buff = List.newBuilder[Output]

    def ansiCodesSupported(): Boolean = true

    def info(msg: String): Unit = buff += Info(encodeTimes(msg))

    def warn(msg: String): Unit = buff += Warn(encodeTimes(msg))

    def debug(msg: String): Unit = buff += Debug(encodeTimes(msg))

    def error(msg: String): Unit = {
      if (msg.startsWith("    at ") || msg.startsWith("    ...")) {
        println(GREY + "Ignoring stacktrace output: " + withoutColor(msg) + NORMAL)
      } else {
        buff += Error(encodeTimes(msg))
      }
    }

    def trace(t: Throwable): Unit = buff += Trace(t.getMessage)

    def handle(event: sbt.testing.Event): Unit = {
      buff += Event(event.status().toString)
    }

    def recordDone(msg: String): Unit = buff += Done(msg)

    def checkOutput(): Unit = {
      @tailrec def minimizeDiff(list1: List[Output], list2: List[Output],
          dropped: Int): (List[Output], List[Output], Int) = {
        (list1, list2) match {
          case (x :: xs, y :: ys) if x == y => minimizeDiff(xs, ys, dropped + 1)
          case _                            => (list1, list2, dropped)
        }
      }

      val expected = expectedOutput(context)
      val actual = buff.result()
      val (expected1, actual1, droppedFront) =
        minimizeDiff(expected, actual, 0)
      val (expected2, actual2, droppedBack) =
        minimizeDiff(expected1.reverse, actual1.reverse, 0)
      val expectedMinimized = expected2.reverse
      val actualMinimized = actual2.reverse

      if (expectedMinimized != actualMinimized) {
        val msg = new StringBuilder
        def appendElems(original: List[Output], minimized: List[Output]): Unit = {
          def appendGreyLine(out: Output): Unit = {
            msg.append(GREY + "  " + withoutColor(out.toString) + NORMAL + "\n")
          }
          original.slice(droppedFront - 3, droppedFront).foreach(appendGreyLine)
          minimized.foreach(out => msg.append("  " + out + "\n"))
          val backIndex = original.size - droppedBack
          original.slice(backIndex, backIndex + 3).foreach(appendGreyLine)
        }
        msg.append(s"JUnit output mismatch with $context:\n")
        msg.append(s"Expected: List(\n")
        appendElems(expected, expectedMinimized)
        msg.append(")\nbut got: List(\n")
        appendElems(actual, actualMinimized)
        msg.append(")")

        throw new Exception(msg.result())
      }
    }

    private def encodeTimes(msg: String): String = {
      val enc1 =
        msg.replaceFirst(", took \\d+(\\.\\d*)? sec", s", took $TIME_TAG sec")
      if (!enc1.contains("Test run finished")) enc1
      else enc1.replaceFirst("\\d+(\\.\\d*)?s", TIME_TAG + "s")
    }
  }

  private def withoutColor(str: String): String =
    str.replaceAll("\\u001B\\[\\d\\d?m", "")
}

object JUnitTest {
  sealed trait Output
  final case class Info(msg: String) extends Output
  final case class Warn(msg: String) extends Output
  final case class Error(msg: String) extends Output
  final case class Debug(msg: String) extends Output
  final case class Trace(msg: String) extends Output
  final case class Event(status: String) extends Output
  final case class Done(msg: String) extends Output
}
