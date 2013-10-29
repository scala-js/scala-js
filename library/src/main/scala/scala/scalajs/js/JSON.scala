/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */



package scala.scalajs.js

object JSON extends Object {
  def parse(text: String, reviver: Function2[Any, Any, Any]): Dynamic = ???
  def parse(text: String): Dynamic = ???

  def stringify(value: Any): String = ???
  def stringify(value: Any, replacer: Function2[String, Any, Any]): String = ???
  def stringify(value: Any, replacer: Array[Any]): String = ???
  def stringify(value: Any, replacer: Function2[String, Any, Any], space: Any): String = ???
  def stringify(value: Any, replacer: Array[Any], space: Any): String = ???
}
