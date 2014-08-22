package java
package lang

import scala.scalajs.js

class Runtime private {
  def exit(status: Int) {
    halt0(status)
  }

  def addShutdownHook(hook: Thread) {}
  def removeShutdownHook(hook: Thread) {}

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
  def freeMemory(): scala.Long = sys.error("Runtime.freeMemory() not implemented")
  def totalMemory(): scala.Long = sys.error("Runtime.totalMemory() not implemented")
  def maxMemory(): scala.Long = Long.MAX_VALUE

  def gc(): Unit = {
    // Ignore
  }

  def runFinalization() {}
  def traceInstructions(on: scala.Boolean) {}
  def traceMethodCalls(on: scala.Boolean) {}

  def load(filename: String): Unit = sys.error("Runtime.load() not implemented")
  def loadLibrary(filename: String): Unit = sys.error("Runtime.loadLibrary() not implemented")
}

object Runtime {
  private val currentRuntime = new Runtime

  def getRuntime() = currentRuntime
}
