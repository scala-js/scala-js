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

package org.scalajs.testing.common

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import java.util.concurrent.atomic.AtomicInteger

import org.junit.Test
import org.junit.Assert._

import org.scalajs.junit.async._

class RunMuxRPCTest {
  import RPCCoreTest._

  lazy val baseX: TestRPC = new TestRPC(baseY)
  lazy val baseY: TestRPC = new TestRPC(baseX)

  val x: RunMuxRPC = new RunMuxRPC(baseX)
  val y: RunMuxRPC = new RunMuxRPC(baseY)

  object eps {
    val call: RPCEndpoint.EP[RunMux[Unit], Unit] = RPCEndpoint[RunMux[Unit], Unit](2)
    val msg: MsgEndpoint.EP[RunMux[Unit]] = MsgEndpoint[RunMux[Unit]](3)
  }

  @Test
  def muxedCall: AsyncResult = await {
    val called = Array.fill(10)(false)

    for (i <- called.indices)
      x.attach(eps.call, i)(_ => called(i) = true)

    called.indices.foldLeft(Future.successful(())) { (prev, i) =>
      for {
        _ <- prev
        _ <- y.call(eps.call, i)(())
      } yield {
        val (needTrue, needFalse) = called.splitAt(i + 1)
        needTrue.foreach(assertTrue _)
        needFalse.foreach(assertFalse _)
      }
    }
  }

  @Test
  def muxedMsg: Unit = {
    val got = Array.fill(10)(false)

    for (i <- got.indices)
      x.attach(eps.msg, i)(_ => got(i) = true)

    for (i <- got.indices) {
      y.send(eps.msg, i)(())
      val (needTrue, needFalse) = got.splitAt(i + 1)
      needTrue.foreach(assertTrue _)
      needFalse.foreach(assertFalse _)
    }
  }

  @Test
  def badRunId: AsyncResult = await {
    x.attach(eps.call, 0)(_ => ())

    y.call(eps.call, 1)(())
      .map(_ => fail("Expected exception"))
      .recover {
        case e: RPCCore.RPCException =>
          val cause = e.getCause()
          assertNotNull(s"Did not get cause: $e", cause)
          assertEquals("Unknown run 1", cause.getMessage())
      }
  }

  @Test
  def detach: Unit = {
    for (i <- 1 to 10)
      x.attach(eps.call, i)(_ => ())

    for (i <- (1 to 10).reverse)
      x.detach(eps.call, i)

    // Attaching now should work again.
    baseX.attach(eps.call)(_ => ())
    baseX.detach(eps.call)
  }
}
