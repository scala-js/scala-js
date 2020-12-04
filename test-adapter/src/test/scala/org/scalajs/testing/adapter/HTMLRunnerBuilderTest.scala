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

import java.nio.file.Files

import org.junit.Test
import org.junit.Assert._

import com.google.common.jimfs.Jimfs

import org.scalajs.jsenv.Input

class HTMLRunnerBuilderTest {

  @Test // #4301
  def relativeOutputFile(): Unit = {
    val fs = Jimfs.newFileSystem()
    val output = fs.getPath("runner.html")
    val artifactsDir = fs.getPath("artifacts")

    Files.createDirectory(artifactsDir)

    HTMLRunnerBuilder.write(output, artifactsDir, "This is only a test", Nil,
        Nil, Nil)
  }

  @Test
  def supportMultipleFileSystems(): Unit = {
    val fs0 = Jimfs.newFileSystem()
    val fs1 = Jimfs.newFileSystem()

    val output = fs0.getPath("runner.html")
    val artifactsDir = fs0.getPath("artifacts")

    val inputFiles = Seq(fs0.getPath("my.js"), fs1.getPath("my.js"))
    val input = inputFiles.map(Input.Script(_))

    Files.createDirectory(artifactsDir)
    inputFiles.foreach(Files.write(_, Array[Byte]()))

    HTMLRunnerBuilder.write(output, artifactsDir, "This is only a test", input,
        Nil, Nil)
  }

  @Test
  def checksOutputsOnSameFileSystems(): Unit = {
    val fs0 = Jimfs.newFileSystem()
    val fs1 = Jimfs.newFileSystem()

    val output = fs0.getPath("runner.html")
    val artifactsDir = fs1.getPath("artifacts")

    Files.createDirectory(artifactsDir)

    try {
      HTMLRunnerBuilder.write(output, artifactsDir, "This is only a test", Nil,
          Nil, Nil)
      fail("expected exception")
    } catch {
      case e: IllegalArgumentException =>
        assertEquals("cannot relativize `artifactsDir` with respect to `output`", e.getMessage())
    }
  }
}
