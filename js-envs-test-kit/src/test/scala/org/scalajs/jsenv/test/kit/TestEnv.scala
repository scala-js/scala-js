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

package org.scalajs.jsenv.test.kit

import scala.concurrent.Future

import java.io._
import java.nio.charset.StandardCharsets
import java.util.concurrent.atomic.AtomicInteger

import org.scalajs.jsenv._

private[kit] class TestEnv private (
    result: Future[Unit],
    outerr: Option[() => InputStream],
    msgs: List[String]) extends JSEnv {

  // Interface for testing.

  def withSuccess(): TestEnv = copy(result = Future.unit)

  def withFailure(t: Throwable): TestEnv = copy(result = Future.failed(t))

  def withHang(): TestEnv = copy(result = Future.never)

  def withOutErr(s: String): TestEnv = {
    val bytes = s.getBytes(StandardCharsets.UTF_8)
    copy(outerr = Some(() => new ByteArrayInputStream(bytes)))
  }

  def withOutErrHang(): TestEnv = {
    def hangStream() = new InputStream {
      // read method that hangs indefinitely.
      def read(): Int = synchronized {
        while (true) wait()
        throw new AssertionError("unreachable code")
      }
    }

    copy(outerr = Some(() => hangStream()))
  }

  def withMsgs(msgs: String*): TestEnv = copy(msgs = msgs.toList)

  private def this() = this(Future.unit, None, Nil)

  private def copy(
      result: Future[Unit] = result,
      outerr: Option[() => InputStream] = outerr,
      msgs: List[String] = msgs) = new TestEnv(result, outerr, msgs)

  // JSEnv interface

  val name: String = "TestEnv"

  def start(input: Input, config: RunConfig): JSRun = {
    require(msgs.isEmpty)
    callOnOutputStream(config)
    new TestRun
  }

  def startWithCom(input: Input, config: RunConfig, onMessage: String => Unit): JSComRun = {
    callOnOutputStream(config)
    msgs.foreach(onMessage)
    new TestRun with JSComRun {
      def send(msg: String): Unit = ()
    }
  }

  private def callOnOutputStream(config: RunConfig): Unit = {
    for {
      factory <- outerr
      onOutputStream <- config.onOutputStream
    } {
      def mkStream = Some(factory())
      onOutputStream(mkStream, mkStream)
    }
  }

  private class TestRun extends JSRun {
    val future: Future[Unit] = result
    def close(): Unit = ()
  }
}

object TestEnv {
  def apply(): TestEnv = new TestEnv()
}
