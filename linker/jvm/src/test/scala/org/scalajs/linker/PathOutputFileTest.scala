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

package org.scalajs.linker

import scala.concurrent.ExecutionContext.Implicits.global

import java.nio.ByteBuffer
import java.nio.file.Files

import org.junit.Test
import org.junit.Assert._

import org.scalajs.junit.async._

import com.google.common.jimfs.Jimfs

import org.scalajs.linker.interface.unstable.OutputFileImpl

@deprecated("Mark deprecated to silence warnings", "never/always")
class PathOutputFileTest {

  @Test // #4301
  def withRelativePath(): AsyncResult = await {
    val file = Jimfs.newFileSystem().getPath("file")

    val impl = OutputFileImpl.fromOutputFile(PathOutputFile(file))

    impl.writeFull(ByteBuffer.wrap(Array())).map { _ =>
      assertTrue(Files.exists(file))
    }
  }

}
