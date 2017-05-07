package org.scalajs.junit.utils

import sbt.testing._
import org.junit.Test

import scala.annotation.tailrec

abstract class JUnitTest {
  import JUnitTest._

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

  protected def frameworkArgss: List[List[String]] = List(
      List(),
      List("-q"),
      List("-a"),
      List("-v"),
      List("-n"),
      List("-n", "-a"),
      List("-n", "-v"),
      List("-n", "-v", "-a"),
      List("-n", "-v", "-c"),
      List("-n", "-v", "-c", "-a"),
      List("-v", "-q"),
      List("-v", "-a"),
      List("-v", "-c")
  )

  protected def expectedOutput(builder: OutputBuilder): OutputBuilder

  @Test def testJUnitOutput(): Unit = {
    for (frameworkArgs <- frameworkArgss) {
      val jUnitFramework = new com.novocode.junit.JUnitFramework()
      val runner =
        jUnitFramework.runner(frameworkArgs.toArray, Array.empty, classLoader)
      val tasks = runner.tasks(Array(new TaskDef(suiteUnderTestName,
        jUnitFramework.fingerprints.head, true, Array.empty)))

      val recorder: JUnitTestRecorder = new JUnitTestRecorder

      // run all tasks and the tasks they generate, needs platform extension
      JUnitTestPlatformImpl.executeLoop(tasks, recorder)

      recorder.recordDone(runner.done())

      val expected = expectedOutput(
          new OutputBuilder(new ArgInfo(frameworkArgs)))

      expected.checkOutput(recorder.result())
    }
  }

  protected final class ArgInfo private[JUnitTest] (args: List[String]) {
    lazy val verbose: Boolean = args.contains("-v")
    lazy val noColor: Boolean = args.contains("-n")
    lazy val quiet: Boolean = args.contains("-q")
    lazy val decodeScalaNames: Boolean = args.contains("-s")
    lazy val logExceptionClass: Boolean = !args.contains("-c")
    lazy val logAssert: Boolean = args.contains("-a")

    override def toString(): String = args.mkString("[", ", ", "]")
  }

  protected final class OutputBuilder private (val argInfo: ArgInfo,
      total: Int, ignored: Int, failed: Int, output: List[Output]) {
    import argInfo._

    private[JUnitTest] def this(argInfo: ArgInfo) = this(argInfo, 0, 0, 0, Nil)

    // Builder methods.

    def success(testName: String): OutputBuilder = append(1, 0, 0)(
        testStartedOutput(testName),
        testFinishedOutput(testName),
        successEvent
    )

    def ignored(testName: String): OutputBuilder = append(0, 1, 0)(
        testIgnoredOutput(testName),
        skippedEvent
    )

    def ignoredClass(): OutputBuilder = append(0, 1, 0)(
        testIgnoredClassOutput,
        skippedEvent
    )

    def assumptionViolated(testName: String): OutputBuilder = append(1, 0, 0)(
        testStartedOutput(testName),
        testAssumptionViolatedOutput(testName),
        skippedEvent,
        testFinishedOutput(testName)
    )

    def exception(testName: String, msg: String, clazz: String): OutputBuilder = {
      append(1, 0, 1)(
          testStartedOutput(testName),
          testExceptionMsgOutput(testName, msg, clazz),
          failureEvent,
          testFinishedOutput(testName)
      )
    }

    def assertion(testName: String, message: String): OutputBuilder = {
      append(1, 0, 1)(
          testStartedOutput(testName),
          testAssertionErrorMsgOutput(testName, message),
          failureEvent,
          testFinishedOutput(testName)
      )
    }

    private def append(t: Int, i: Int, f: Int)(out: Output*) = {
      new OutputBuilder(argInfo, total + t, ignored + i, failed + f, output ++ out)
    }

    // Test method.

    private[JUnitTest] def checkOutput(actual: List[Output]): Unit = {
      val expected = (
          testRunStartedOutput :: output :::
          List(testRunFinishedOutput(total, ignored, failed), done)
      )

      @tailrec def minimizeDiff(list1: List[Output], list2: List[Output],
          dropped: Int): (List[Output], List[Output], Int) = {
        (list1, list2) match {
          case (x :: xs, y :: ys) if x == y => minimizeDiff(xs, ys, dropped + 1)
          case _                            => (list1, list2, dropped)
        }
      }
  
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
  
        msg.append(s"JUnit output mismatch with $argInfo:\n")
        msg.append(s"Expected: List(\n")
        appendElems(expected, expectedMinimized)
        msg.append(")\nbut got: List(\n")
        appendElems(actual, actualMinimized)
        msg.append(")")
  
        throw new Exception(msg.result())
      }
    }

    // Text builders.

    private def done: Output = Done("")

    private def testStartedOutput(method: String): Output = {
      infoIfOrElseDebug(verbose,
          s"Test $formattedTestClass.${cyan(method)} started")
    }

    private def testIgnoredOutput(method: String): Output =
      Info(s"Test $formattedTestClass.${cyan(method)} ignored")

    private def testIgnoredClassOutput: Output =
      Info(s"Test $formattedTestClass ignored")

    private def testExceptionMsgOutput(method: String, msg: String,
        exClass: String): Output = {
      val exClassStr = exceptionClassInfo(show = logExceptionClass, exClass)
      Error(s"Test $formattedTestClass.${red(method)} " +
          s"failed: $exClassStr$msg, took $TIME_TAG sec")
    }

    private def testAssertionErrorMsgOutput(method: String, msg: String): Output = {
      val assertClass = exceptionClassInfo(show = logExceptionClass && logAssert,
          "java.lang.AssertionError")
      Error(s"Test $formattedTestClass.${red(method)} failed: $assertClass" +
          s"$msg, took $TIME_TAG sec")
    }

    private def testAssumptionViolatedOutput(method: String): Output = {
      val exceptionStr = exceptionClassInfo(logExceptionClass,
          "org.junit.internal.AssumptionViolatedException")
      Warn(s"Test assumption in test $formattedTestClass.${red(method)}" +
          s" failed: $exceptionStr" +
          s"This assume should not pass, took $TIME_TAG sec")
    }

    private def testFinishedOutput(method: String): Output = {
      Debug(s"Test $formattedTestClass.${cyan(method)} finished, " +
          s"took $TIME_TAG sec")
    }

    private def testRunStartedOutput: Output =
      infoIfOrElseDebug(verbose, blue("Test run started"))

    private def testRunFinishedOutput(expectedTotal: Int, expectedIgnored: Int,
        expectedFail: Int): Output = {
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

    private def skippedEvent: Output = Event("Skipped")
    private def successEvent: Output = Event("Success")
    private def failureEvent: Output = Event("Failure")

    private def formattedTestClass: String =
      formatClass(suiteUnderTestName, yellow)

    private def exceptionClassInfo(show: Boolean, fullName: String) = {
      if (show) formatClass(fullName, red) + ": "
      else ""
    }

    private def formatClass(fullName: String, color: String => String) = {
      val (packAndDot, cls) = fullName.splitAt(fullName.lastIndexOf('.') + 1)
      packAndDot + color(cls)
    }

    private def infoIfOrElseDebug(p: Boolean, msg: String): Output =
      if (p) Info(msg)
      else Debug(msg)

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

  private class JUnitTestRecorder extends Logger with EventHandler {
    private val buff = List.newBuilder[Output]

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

    def result(): List[Output] = buff.result()

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
  private sealed trait Output
  private final case class Info(msg: String) extends Output
  private final case class Warn(msg: String) extends Output
  private final case class Error(msg: String) extends Output
  private final case class Debug(msg: String) extends Output
  private final case class Trace(msg: String) extends Output
  private final case class Event(status: String) extends Output
  private final case class Done(msg: String) extends Output
}
