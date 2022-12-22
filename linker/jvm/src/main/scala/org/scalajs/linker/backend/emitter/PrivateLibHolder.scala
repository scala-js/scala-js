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

import org.scalajs.ir

import org.scalajs.linker.interface.IRFile
import org.scalajs.linker.standard.MemIRFileImpl

object PrivateLibHolder {
  private val sjsirPaths = Seq(
      "org/scalajs/linker/runtime/RuntimeLong.sjsir",
      "org/scalajs/linker/runtime/RuntimeLong$.sjsir",
      "org/scalajs/linker/runtime/UndefinedBehaviorError.sjsir",
      "scala/scalajs/js/JavaScriptException.sjsir"
  )

  val files: Seq[IRFile] = {
    for (path <- sjsirPaths) yield {
      val name = path.substring(path.lastIndexOf('/') + 1)
      new MemIRFileImpl(
          path = path,
          version = ir.Version.fromInt(0), // never changes
          content = readResource(name)
      )
    }
  }

  private def readResource(name: String): Array[Byte] = {
    val inputStream = getClass().getResourceAsStream(name)
    assert(inputStream != null, s"couldn't load $name from resources")

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
