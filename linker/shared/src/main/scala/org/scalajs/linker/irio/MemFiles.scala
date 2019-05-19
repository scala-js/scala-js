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

package org.scalajs.linker.irio

import java.io._
import java.nio.charset.StandardCharsets

final class WritableMemVirtualBinaryFile extends WritableVirtualBinaryFile {
  private var _content: Array[Byte] = _

  def content: Array[Byte] = _content

  def outputStream: OutputStream = new ByteArrayOutputStream {
    override def close(): Unit = {
      super.close()
      _content = this.toByteArray
    }
  }
}
