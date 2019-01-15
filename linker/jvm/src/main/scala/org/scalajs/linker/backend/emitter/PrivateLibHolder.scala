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

package org.scalajs.linker.backend.emitter

import java.io._

import org.scalajs.linker.irio._

object PrivateLibHolder {
  private val relativeDir = "scala/scalajs/runtime/"
  private val sjsirNames = Seq("RuntimeLong.sjsir", "RuntimeLong$.sjsir")

  val files: Seq[VirtualScalaJSIRFile] = {
    sjsirNames.flatMap { name =>
      val inputStream = getClass().getResourceAsStream(name)
      if (inputStream == null) {
        // This should happen only in our own build. How do we assert that?
        Nil
      } else {
        val content = readInputStreamAsByteArray(inputStream)
        val relativePath = relativeDir + name
        val version = Some("") // this indicates that the file never changes
        val vf = new MemVirtualSerializedScalaJSIRFile(
            relativePath, relativePath, content, version)
        vf :: Nil
      }
    }
  }

  private def readInputStreamAsByteArray(
      inputStream: InputStream): Array[Byte] = {
    try {
      val output = new ByteArrayOutputStream
      val buf = new Array[Byte](65536) // enough for all our resources
      while ({
        val read = inputStream.read(buf)
        if (read > 0) {
          output.write(buf, 0, read)
        }
        read != -1
      }) {}
      output.toByteArray()
    } finally {
      inputStream.close()
    }
  }

}
