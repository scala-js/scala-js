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

import java.util.EnumSet

import org.junit.Test
import org.junit.Assert._

import org.scalajs.junit.async._

import com.google.common.jimfs.{Jimfs, Configuration}

import org.scalajs.linker.interface.unstable.OutputDirectoryImpl

class PathOutputDirectoryTest {

  private val dummyContent =
    "Hello World, I'm a file".getBytes(StandardCharsets.UTF_8)

  @Test
  def avoidUnnecessaryWrite(): AsyncResult = await {
    val dir = Jimfs.newFileSystem().getPath("tmp").toAbsolutePath()
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
  def readFull(): AsyncResult = await {
    val dir = Jimfs.newFileSystem().getPath("tmp").toAbsolutePath()
    Files.createDirectory(dir)

    val fileName = "file.js"
    val filePath = dir.resolve(fileName)

    Files.write(filePath, dummyContent)

    val readOp = OutputDirectoryImpl
      .fromOutputDirectory(PathOutputDirectory(dir))
      .readFull(fileName)

    readOp.map { buf =>
      assertEquals(ByteBuffer.wrap(dummyContent), buf)
    }
  }

  @Test // #4212
  def allowGroupOthersReadPosixFileSystem(): AsyncResult = await {
    val config = Configuration.unix().toBuilder()
      .setAttributeViews("basic", "posix")
      .build()

    val dir = Jimfs.newFileSystem(config).getPath("/tmp") // we forced Unix, so /tmp is fine
    Files.createDirectory(dir)

    val fileName = "file.js"
    val filePath = dir.resolve(fileName)

    val writeOp = OutputDirectoryImpl
      .fromOutputDirectory(PathOutputDirectory(dir))
      .writeFull(fileName, ByteBuffer.wrap(dummyContent))

    writeOp.map { _ =>
      import java.nio.file.attribute.PosixFilePermission._

      val gotPerms = Files.getPosixFilePermissions(filePath)
      val wantPerms =
        EnumSet.of(OWNER_READ, OWNER_WRITE, GROUP_READ, OTHERS_READ)

      assertEquals(wantPerms, gotPerms)
    }
  }
}
