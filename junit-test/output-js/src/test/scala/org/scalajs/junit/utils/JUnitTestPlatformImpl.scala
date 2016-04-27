package org.scalajs.junit.utils

import sbt.testing._

import scala.scalajs.js

object JUnitTestPlatformImpl {

  def getClassLoader: ClassLoader =
    new org.scalajs.testinterface.ScalaJSClassLoader(js.Dynamic.global)

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
