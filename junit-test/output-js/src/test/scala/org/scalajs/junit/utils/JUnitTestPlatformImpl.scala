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

import sbt.testing._

object JUnitTestPlatformImpl {

  def getClassLoader: ClassLoader = null

  def executeLoop(tasks: Array[Task], recorder: Logger with EventHandler): Unit = {
    if (tasks.nonEmpty) {
      executeLoop(tasks.flatMap { task =>
        /* This is a hack, assuming the asynchronous continuation will be called before
         * the outer call returns. This is terrible in general, but we do not have
         * another choice, since JUnit does not support async testing.
         * However, since JUnit does not support async testing, this is a safe
         * assumption to make (modulo JUnit implementation details, which we control).
         * If JUnit ever were to support async testing, we could also change this here.
         */
        task.execute(recorder, Array(recorder))
        Array.empty[Task]
      }, recorder)
    }
  }
}
