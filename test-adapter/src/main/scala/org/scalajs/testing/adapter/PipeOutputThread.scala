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

package org.scalajs.testing.adapter

import scala.annotation.tailrec

import java.io._

// !!! Duplicate code with sbtplugin/PipeOutputThread.scala.
private[adapter] final class PipeOutputThread(from: InputStream, to: OutputStream) extends Thread {
  override def run(): Unit = {
    /* Copied from scala.sys.process.BasicIO.transferFully, except that we
     * don't have the try/catch around `flush()` because we do not use this
     * method for stdin (only for stdout and stderr).
     */
    try {
      val buffer = new Array[Byte](8192)
      @tailrec def loop(): Unit = {
        val byteCount = from.read(buffer)
        if (byteCount > 0) {
          to.write(buffer, 0, byteCount)
          to.flush()
          loop()
        }
      }
      loop()
    } catch {
      case _: InterruptedIOException => ()
    }
    from.close()
  }
}
