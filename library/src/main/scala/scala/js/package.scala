/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala

package object js extends js.GlobalScope {
  val NaN: Number = sys.error("stub")
  val Infinity: Number = sys.error("stub")

  def eval(s: String): Any = sys.error("stub")

  def parseInt(s: String): Number = sys.error("stub")
  def parseInt(s: String, radix: Number): Number = sys.error("stub")
  def parseFloat(s: String): Number = sys.error("stub")
}
