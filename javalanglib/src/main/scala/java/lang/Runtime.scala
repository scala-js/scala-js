package java
package lang

import scala.scalajs.js

class Runtime private {
  def exit(status: Int) {
    halt0(status)
  }

  //def addShutdownHook(hook: Thread): Unit
  //def removeShutdownHook(hook: Thread): Unit

  def halt(status: Int) {
    halt0(status)
  }

  private def halt0(status: Int): Unit = {
    val envInfo = scala.scalajs.runtime.environmentInfo

    if (envInfo != js.undefined && envInfo != null &&
        js.typeOf(envInfo.exitFunction) == "function") {
      envInfo.exitFunction(status)
      throw new IllegalStateException("__ScalaJSEnv.exitFunction returned")
    } else {
      // We don't have an exit function. Fail
      throw new SecurityException("Cannot terminate a JavaScript program. " +
          "Define a JavaScript function `__ScalaJSEnv.exitFunction` to " +
          "be called on exit.")
    }
  }

  def availableProcessors() = 1
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

  def getRuntime() = currentRuntime
}
