package java.lang

/* We need a constructor to create SingleThread in the companion object, but
 * we don't want user code doing a 'new Thread()' to link, because that could
 * be confusing.
 * So we use a binary signature that no Java source file can ever produce.
 */
class Thread private (dummy: Unit) extends Runnable {
  private[this] var name: String = "main" // default name of the main thread

  def run(): Unit = ()

  final def setName(name: String): Unit =
    this.name = name

  final def getName(): String =
    this.name

  def getStackTrace(): Array[StackTraceElement] =
    scala.scalajs.runtime.StackTrace.getCurrentStackTrace()
}

object Thread {
  private[this] val SingleThread = new Thread(())

  def currentThread(): Thread = SingleThread
}
