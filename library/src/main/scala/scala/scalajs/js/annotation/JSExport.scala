/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.scalajs.js.annotation

/** Specifies that the given entity should be exported for use in raw JS */
class JSExport private (name: Option[String]) extends scala.annotation.StaticAnnotation {
  def this() = this(None)
  def this(name: String) = this(Some(name))
}
