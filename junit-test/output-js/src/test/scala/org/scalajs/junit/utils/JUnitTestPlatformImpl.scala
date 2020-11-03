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

import scala.scalajs.js

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import sbt.testing._

object JUnitTestPlatformImpl {

  def getClassLoader: ClassLoader = null

  def executeLoop(tasks: Array[Task], recorder: Logger with EventHandler): Future[Unit] = {
    if (tasks.isEmpty) {
      Future.successful(())
    } else {
      Future
        .traverse(tasks.toList)(executeTask(_, recorder))
        .flatMap(newTasks => executeLoop(newTasks.flatten.toArray, recorder))
    }
  }

  private def executeTask(task: Task, recorder: Logger with EventHandler): Future[Array[Task]] = {
    val p = Promise[Array[Task]]()
    task.execute(recorder, Array(recorder), p.success _)
    p.future
  }

  def writeLines(lines: List[String], file: String): Unit =
    throw new UnsupportedOperationException("Writing is only supported on the JVM.")

  def readLines(file: String): List[String] = {
    val fs = js.Dynamic.global.require("fs")
    val c = fs.readFileSync(file, "utf8").asInstanceOf[String]
    c.split('\n').toList
  }
}
