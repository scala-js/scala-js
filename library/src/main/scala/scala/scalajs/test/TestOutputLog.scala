package scala.scalajs.test

trait TestOutputLog {

  def info(message: String): Unit
  def error(message: String): Unit

}
