/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala

package object js extends js.GlobalScope {
  val NaN: Number = ???
  val Infinity: Number = ???

  def eval(x: String): Any = ???

  def parseInt(s: String, radix: Number): Number = ???
  def parseInt(s: String): Number = ???
  def parseFloat(string: String): Number = ???

  def isNaN(number: Number): Boolean = ???
  def isFinite(number: Number): Boolean = ???

  def decodeURI(encodedURI: String): String = ???
  def decodeURIComponent(encodedURIComponent: String): String = ???
  def encodeURI(uri: String): String = ???
  def encodeURIComponent(uriComponent: String): String = ???
}
