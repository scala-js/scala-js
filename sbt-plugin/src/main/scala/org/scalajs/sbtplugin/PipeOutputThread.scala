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

package org.scalajs.sbtplugin

import scala.annotation.tailrec

import java.io._

// !!! Duplicate code with adapter/PipeOutputThread.scala.
private[sbtplugin] object PipeOutputThread {
  def start(from: InputStream, to: OutputStream): Thread = {
    val thread = new PipeOutputThread(from, to)
    thread.start()
    thread
  }
}

private final class PipeOutputThread(from: InputStream, to: OutputStream) extends Thread {
  override def run(): Unit = {
    try {
      val buffer = new Array[Byte](8192)
      @tailrec def loop(): Unit = {
        val byteCount = from.read(buffer)
        if (byteCount > 0) {
          to.write(buffer, 0, byteCount)
          to.flush() // if the sender flushed (which we can't tell), we need to propagate the flush
          loop()
        }
      }
      loop()
    } finally {
      from.close()
    }
  }
}
