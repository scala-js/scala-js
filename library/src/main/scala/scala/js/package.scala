/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala

package object js extends JSGlobalScope {
  val NaN: JSNumber = sys.error("stub")
  val Infinity: JSNumber = sys.error("stub")

  def eval(s: JSString): JSAny = sys.error("stub")

  def parseInt(s: JSString): JSNumber = sys.error("stub")
  def parseInt(s: JSString, radix: JSNumber): JSNumber = sys.error("stub")
  def parseFloat(s: JSString): JSNumber = sys.error("stub")
}
