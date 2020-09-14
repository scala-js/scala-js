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
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import org.junit.Test
import org.junit.Assert._

import org.scalajs.junit.async._

import com.google.common.jimfs.Jimfs

import org.scalajs.linker.interface.unstable.OutputDirectoryImpl

class PathOutputDirectoryTest {

  private val dummyContent =
    "Hello World, I'm a file".getBytes(StandardCharsets.UTF_8)

  @Test
  def avoidUnnecessaryWrite(): AsyncResult = await {
    val dir = Jimfs.newFileSystem().getPath("/tmp")
    Files.createDirectory(dir)

    val fileName = "file.js"
    val filePath = dir.resolve(fileName)

    // Simulate a file from a previous run.
    Files.write(filePath, dummyContent)

    val lastModifiedBefore = Files.getLastModifiedTime(filePath)

    val writeOp = OutputDirectoryImpl
      .fromOutputDirectory(PathOutputDirectory(dir))
      .writeFull(fileName, ByteBuffer.wrap(dummyContent))

    writeOp.map { _ =>
      val lastModifiedAfter = Files.getLastModifiedTime(filePath)
      assertEquals(lastModifiedBefore, lastModifiedAfter)
    }
  }
}
