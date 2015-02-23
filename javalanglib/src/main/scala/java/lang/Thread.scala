package java.lang

/* We need a constructor to create SingleThread in the companion object, but
 * we don't want user code doing a 'new Thread()' to link, because that could
 * be confusing.
 * So we use a binary signature that no Java source file can ever produce.
 */
class Thread private (dummy: Unit) extends Runnable {
  def run(): Unit = ()

  def getStackTrace(): Array[StackTraceElement] =
    scala.scalajs.runtime.StackTrace.getCurrentStackTrace()
}

object Thread {
  private[this] val SingleThread = new Thread(())

  def currentThread(): Thread = SingleThread
}
