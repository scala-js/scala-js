/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.js

object JSON extends Object {
  def parse(text: String, reviver: Function2[Any, Any, Any]): Dynamic = ???
  def parse(text: String): Dynamic = ???

  def stringify(value: Any): String = ???
  def stringify(value: Any, replacer: Function2[String, Any, Any]): String = ???
  def stringify(value: Any, replacer: Array[Any]): String = ???
  def stringify(value: Any, replacer: Function2[String, Any, Any], space: Any): String = ???
  def stringify(value: Any, replacer: Array[Any], space: Any): String = ???
}
