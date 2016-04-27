package org.scalajs.junit.utils

import sbt.testing._

object JUnitTestPlatformImpl {

  def getClassLoader: ClassLoader = getClass.getClassLoader

  def executeLoop(tasks: Array[Task], recorder: Logger with EventHandler): Unit = {
    if (tasks.nonEmpty) {
      executeLoop(tasks.flatMap(_.execute(recorder, Array(recorder))), recorder)
    }
  }
}
