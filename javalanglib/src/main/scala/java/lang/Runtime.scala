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

package java.lang

import scala.scalajs.js

class Runtime private {
  def exit(status: Int): Unit =
    halt(status)

  //def addShutdownHook(hook: Thread): Unit
  //def removeShutdownHook(hook: Thread): Unit

  def halt(status: Int): Unit = {
    val envInfo = EnvironmentInfo.envInfo

    envInfo.flatMap(_.exitFunction).fold {
      // We don't have an exit function. Fail
      throw new SecurityException("Cannot terminate a JavaScript program. " +
          "Define a JavaScript function `__ScalaJSEnv.exitFunction` to " +
          "be called on exit.")
    } { exitFunction =>
      exitFunction(status)
      throw new IllegalStateException("__ScalaJSEnv.exitFunction returned")
    }
  }

  def availableProcessors(): Int = 1
  //def freeMemory(): scala.Long
  //def totalMemory(): scala.Long
  //def maxMemory(): scala.Long

  def gc(): Unit = {
    // Ignore
  }

  //def runFinalization(): Unit
  //def traceInstructions(on: scala.Boolean): Unit
  //def traceMethodCalls(on: scala.Boolean): Unit

  //def load(filename: String): Unit
  //def loadLibrary(filename: String): Unit
}

object Runtime {
  private val currentRuntime = new Runtime

  def getRuntime(): Runtime = currentRuntime
}
