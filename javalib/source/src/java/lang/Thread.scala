package java.lang

class Thread extends Runnable {
  def run() {}
}

object Thread {
  private[this] val SingleThread = new Thread

  def currentThread(): Thread = SingleThread
}
