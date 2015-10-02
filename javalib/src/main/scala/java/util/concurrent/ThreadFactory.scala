package java.util.concurrent

trait ThreadFactory {
  def newThread(r: Runnable): Thread
}
