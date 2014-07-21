package java.util.concurrent

trait Executor {
  def execute(command: Runnable): Unit
}
