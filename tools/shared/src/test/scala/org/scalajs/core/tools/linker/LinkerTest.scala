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

package org.scalajs.core.tools.linker

import org.junit.Test
import org.junit.Assert._

import org.scalajs.core.tools.logging.NullLogger
import org.scalajs.core.tools.io._
import org.scalajs.core.tools.linker._

class LinkerTest {

  /** This test exposes a problem where a linker in error state is called
   *  multiple times and ends up thinking it is being used concurrently.
   */
  @Test
  def clean_linking_state(): Unit = {
    class DummyException extends Exception

    val badSeq = new IndexedSeq[VirtualScalaJSIRFile] {
      def apply(x: Int): VirtualScalaJSIRFile = throw new DummyException()
      def length: Int = throw new DummyException()
    }

    val linker = StandardLinker(StandardLinker.Config())

    def callLink(): Unit =
      linker.link(badSeq, Nil, WritableMemVirtualJSFile("some_file"), NullLogger)

    // Call first time. Get exception from badSeq.
    try {
      callLink()
      fail("Expected DummyException")
    } catch {
      case e: DummyException => // ok.
    }

    def callInFailedState(): Unit = {
      try {
        callLink()
        fail("Expected IllegalStateException")
      } catch {
        case e: IllegalStateException =>
          if (e.getMessage.contains("concurrent")) {
            fail("Found bad message in exception: " + e.getMessage)
          }
      }
    }

    for (_ <- 1 to 4) callInFailedState()
  }

}
