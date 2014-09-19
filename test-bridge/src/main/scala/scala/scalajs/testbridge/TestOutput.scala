/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.testbridge

trait TestOutput {

  type Color

  val errorColor: Color
  val successColor: Color
  val infoColor: Color

  def color(message: String, color: Color): String

  def error(message: String, stack: Array[StackTraceElement]): Unit
  def error(message: String): Unit
  def failure(message: String, stack: Array[StackTraceElement]): Unit
  def failure(message: String): Unit
  def succeeded(message: String): Unit
  def skipped(message: String): Unit
  def pending(message: String): Unit
  def ignored(message: String): Unit
  def canceled(message: String): Unit

  def log: TestOutputLog

}
