package java
package lang

class Runtime private {
  def exit(status: Int) {
    halt0(status)
  }

  def addShutdownHook(hook: Thread) {}
  def removeShutdownHook(hook: Thread) {}

  def halt(status: Int) {
    halt0(status)
  }

  @native private def halt0(status: Int): Unit

  def availableProcessors() = 1
  def freeMemory(): scala.Long = sys.error("Runtime.freeMemory() not implemented")
  def totalMemory(): scala.Long = sys.error("Runtime.totalMemory() not implemented")
  def maxMemory(): scala.Long = Long.MAX_VALUE

  @native def gc(): Unit

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
