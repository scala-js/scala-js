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

package org.scalajs.junit.utils

import scala.annotation.tailrec

import sbt.testing._
import org.junit.Test
import org.junit.Assert.fail

import org.scalajs.junit.async._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

abstract class JUnitTest {
  import JUnitTest._

  // appropriate class loader for platform, needs platform extension
  private val classLoader: ClassLoader =
    JUnitTestPlatformImpl.getClassLoader

  private val recordOutput = sys.props.contains("org.scalajs.junit.utils.record")
  private val recordDir = "junit-test/outputs/"

  private val suiteUnderTestName = {
    val myName = getClass.getName
    assert(myName.endsWith("Assertions"))
    myName.stripSuffix("Assertions")
  }

  private val frameworkArgss: List[List[Char]] = List(
    List(),
    List('a'),
    List('v'),
    List('n'),
    List('n', 'a'),
    List('n', 'v'),
    List('n', 'v', 'a'),
    List('n', 'v', 'c'),
    List('n', 'v', 'c', 'a'),
    List('v', 'a'),
    List('v', 'c'),
    List('v', 's'),
    List('v', 's', 'n')
  )

  @Test def testJUnitOutput(): AsyncResult = await {
    val futs = for (args <- frameworkArgss) yield {
      for {
        rawOut <- runTests(args.map("-" + _))
      } yield {
        val out = postprocessOutput(rawOut)

        val file = recordPath(args)

        if (recordOutput) {
          val lines = out.map(Output.serialize)
          JUnitTestPlatformImpl.writeLines(lines, file)
        } else {
          val lines = JUnitTestPlatformImpl.readLines(file)
          val want = lines.map(Output.deserialize)

          if (want != out) {
            fail(s"Bad output (args: $args)\n\nWant:\n${want.mkString("\n")}\n\nGot:\n${out.mkString("\n")}\n\n")
          }
        }
      }
    }

    Future.sequence(futs).map(_ => ())
  }

  private def runTests(args: List[String]): Future[List[Output]] = {
    val recorder = new JUnitTestRecorder
    val framework = new com.novocode.junit.JUnitFramework()
    val runner = framework.runner(args.toArray, Array.empty, classLoader)
    val tasks = runner.tasks(Array(new TaskDef(suiteUnderTestName,
        framework.fingerprints.head, true, Array.empty)))

    // run all tasks and the tasks they generate, needs platform extension
    for {
      _ <- JUnitTestPlatformImpl.executeLoop(tasks, recorder)
    } yield {
      recorder.recordDone(runner.done())
      recorder.result()
    }
  }

  private def postprocessOutput(out: List[Output]): List[Output] = {
    val noTime = out.map(replaceTime)
    val noStack = noTime.filterNot(isStackTrace)
    orderByTestName(noStack)
  }

  private def isStackTrace(out: Output): Boolean = out match {
    case Log(_, msg) => msg.startsWith("    at ") || msg.startsWith("    ...")
    case _           => false
  }

  /** Orders test output by test (method) name.
   *
   *  This assumes the total output is of the following form:
   *  <no test> ...
   *  <test 1>
   *  <no test | test 1> ...
   *  <test 1>
   *  <test 2>
   *  <no test | test 2> ...
   *  <test 2>
   *  ...
   *  <no test> ...
   */
  private def orderByTestName(out: List[Output]): List[Output] = {
    val (prefix, rem) = out.span(o => testName(o).isEmpty)

    @tailrec
    def makeChunks(rem: List[Output],
        chunks: Map[String, List[Output]]): (Map[String, List[Output]], List[Output]) = {
      if (rem.isEmpty) {
        (chunks, rem)
      } else {
        testName(rem.head) match {
          case Some(name) =>
            require(!chunks.contains(name), s"Got chunks for test $name twice")

            val i = rem.lastIndexWhere(o => testName(o) == Some(name))
            val (chunk, newRem) = rem.splitAt(i + 1)

            require(chunk.forall(o => testName(o).forall(_ == name)),
                s"Test chunks interleaved: $chunk")

            makeChunks(newRem, chunks + (name -> chunk))

          case None =>
            (chunks, rem)
        }
      }
    }

    val (chunks, suffix) = makeChunks(rem, Map.empty)

    require(suffix.forall(o => testName(o).isEmpty),
        s"Got unhandlable suffix:\n${suffix.mkString("\n")}")

    prefix ++ chunks.toSeq.sortBy(_._1).flatMap(_._2) ++ suffix
  }

  private def recordPath(args: List[Char]): String = {
    val fname = getClass.getName.replace('.', '/') + "_" + args.mkString("") + ".txt"
    recordDir + "/" + fname
  }

  private val testNamePrefix = suiteUnderTestName + "."

  private def testName(out: Output): Option[String] = out match {
    case Log(_, cmsg) =>
      val msg = withoutColor(cmsg)

      val s = msg.indexOf(testNamePrefix)
      if (s == -1) {
        None
      } else {
        val e = msg.indexOf(' ', s)
        if (e == -1) {
          throw new AssertionError("Unknown message format, cannot extract testName: " + msg)
        }

        Some(msg.substring(s + testNamePrefix.length, e))
      }

    case Event(_, name, _, _) =>
      if (name == suiteUnderTestName) None
      else if (name.startsWith(testNamePrefix)) Some(name.stripPrefix(testNamePrefix))
      else throw new AssertionError(s"Unknown test name: $name")

    case Done(_) => None
  }

  private val colorRE = """\u001B\[\d\d?m""".r
  private val timeRE = """\d+(\.\d+)?( sec|s)\b""".r

  def replaceTime(out: Output): Output = out match {
    case Log(level, msg) =>
      Log(level, timeRE.replaceAllIn(msg, "<TIME>"))

    case out => out
  }

  def withoutColor(str: String): String = colorRE.replaceAllIn(str, "")

  class JUnitTestRecorder extends Logger with EventHandler {
    private val buf = List.newBuilder[Output]

    val ansiCodesSupported: Boolean = true

    def info(msg: String): Unit = buf += Log('i', msg)

    def warn(msg: String): Unit = buf += Log('w', msg)

    def debug(msg: String): Unit = buf += Log('d', msg)

    def error(msg: String): Unit = buf += Log('e', msg)

    def trace(t: Throwable): Unit =
      throw new UnsupportedOperationException("trace is not supported")

    def handle(event: sbt.testing.Event): Unit = {
      val testName: String = event.selector() match {
        case s: TestSelector =>
          s.testName()

        case _ =>
          throw new AssertionError("unexpected selector")
      }

      buf += Event(
        event.status(),
        testName,
        if (event.throwable.isEmpty) None else Some(event.throwable.get.toString),
        event.duration >= 0
      )
    }

    def recordDone(msg: String): Unit = buf += Done(msg)

    def result(): List[Output] = buf.result()
  }
}

object JUnitTest {
  sealed trait Output
  final case class Log(level: Char, msg: String) extends Output
  final case class Done(msg: String) extends Output

  final case class Event(
      status: Status,
      testName: String,
      throwableToString: Option[String],
      durationPopulated: Boolean
  ) extends Output

  object Output {
    // We need something that does not occur in test names nor in Throwable.toString.
    final val separator = "::"

    def deserialize(line: String): Output = line.toList match {
      case 'l' :: level :: msg => Log(level, msg.mkString(""))
      case 'd' :: msg          => Done(msg.mkString(""))

      case 'e' :: s :: tail =>
        val rest: Array[String] = tail.mkString("").split(separator, -1)
        val testName: String = rest(0)
        val throwableToString: String = rest(1)
        val durationPopulated: Boolean = rest(2).toBoolean
        Event(
          status = Status.values()(s - '0'),
          testName,
          throwableToString = if (throwableToString.isEmpty) None else Some(throwableToString),
          durationPopulated
        )
    }

    def serialize(o: Output): String = o match {
      case Log(level, msg)   => "l" + level + msg
      case Done(msg)         => "d" + msg
      case Event(s, n, t, d) => "e" + s.ordinal + n + separator + t.getOrElse("") + separator + d
    }
  }
}
