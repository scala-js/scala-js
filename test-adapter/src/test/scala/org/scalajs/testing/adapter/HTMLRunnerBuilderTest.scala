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

import com.google.common.jimfs.Jimfs

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

}
