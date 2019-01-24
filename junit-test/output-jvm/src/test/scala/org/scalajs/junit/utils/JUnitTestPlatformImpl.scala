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

import scala.annotation.tailrec

import scala.collection.JavaConverters._

import scala.concurrent.Future

import java.nio.file._
import java.nio.charset.StandardCharsets.UTF_8

import sbt.testing._

object JUnitTestPlatformImpl {

  def getClassLoader: ClassLoader = getClass.getClassLoader

  @tailrec
  def executeLoop(tasks: Array[Task], recorder: Logger with EventHandler): Future[Unit] = {
    if (tasks.nonEmpty) {
      executeLoop(tasks.flatMap(_.execute(recorder, Array(recorder))), recorder)
    } else {
      Future.successful(())
    }
  }

  def writeLines(lines: List[String], file: String): Unit =
    Files.write(Paths.get(file), lines.toIterable.asJava, UTF_8)

  def readLines(file: String): List[String] =
    Files.readAllLines(Paths.get(file), UTF_8).asScala.toList
}
