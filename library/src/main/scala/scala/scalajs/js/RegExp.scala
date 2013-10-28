/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.scalajs.js

class RegExp protected () extends Object {
  def this(pattern: String, flags: String) = this()
  def this(pattern: String) = this()

  val source: String = ???
  val global: Boolean = ???
  val ignoreCase: Boolean = ???
  val multiline: Boolean = ???

  var lastIndex: Number = ???

  def exec(string: String): RegExp.ExecResult = ???
  def test(string: String): Boolean = ???
}

object RegExp extends Object {
  def apply(pattern: String, flags: String): RegExp = ???
  def apply(pattern: String): RegExp = ???

  trait ExecResult extends Array[String] {
    var index: Number = _
    var input: String = _
  }
}
