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
import java.nio.file.Paths

import java.util.EnumSet

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

  @Test
  def sanePosixFilePermissions(): AsyncResult = await {
    import java.nio.file.attribute.PosixFilePermission._

    val tmpDir = Files.createTempDirectory("test-dir")
    val fileName = "file.js"
    val filePath = tmpDir.resolve(fileName)

    val writeOp = OutputDirectoryImpl
      .fromOutputDirectory(PathOutputDirectory(tmpDir))
      .writeFull(fileName, ByteBuffer.wrap(dummyContent))

    writeOp.map { _ =>
      try {
        val filePerms = Files.getPosixFilePermissions(filePath)

        val requiredPerms = EnumSet.of(
          OWNER_READ, OWNER_WRITE, GROUP_READ, OTHERS_READ)

        assertTrue(filePerms.containsAll(requiredPerms))
      } catch {
        case _: UnsupportedOperationException =>
      } finally {
        filePath.toFile().delete()
        tmpDir.toFile().delete()
      }
    }
  }
}
