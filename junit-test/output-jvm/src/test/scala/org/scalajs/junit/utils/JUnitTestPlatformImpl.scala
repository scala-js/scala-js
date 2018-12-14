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

package org.scalajs.junit.utils

import scala.collection.JavaConverters._

import java.nio.file._
import java.nio.charset.StandardCharsets.UTF_8

import sbt.testing._

object JUnitTestPlatformImpl {

  def getClassLoader: ClassLoader = getClass.getClassLoader

  def executeLoop(tasks: Array[Task], recorder: Logger with EventHandler): Unit = {
    if (tasks.nonEmpty) {
      executeLoop(tasks.flatMap(_.execute(recorder, Array(recorder))), recorder)
    }
  }

  def writeLines(lines: List[String], file: String): Unit =
    Files.write(Paths.get(file), lines.toIterable.asJava, UTF_8)

  def readLines(file: String): List[String] =
    Files.readAllLines(Paths.get(file), UTF_8).asScala.toList
}
