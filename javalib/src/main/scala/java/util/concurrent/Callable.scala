package java.util.concurrent

trait Callable[V] {
  def call(): V
}
