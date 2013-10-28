/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.scalajs.js.annotation

/** Specifies the JavaScript name of an entity.
 *
 *  @see [[http://lampwww.epfl.ch/~doeraene/scala-js/doc/js-interoperability.html Guide to JavaScript interoperability in Scala.js]]
 */
class JSName(name: String) extends scala.annotation.StaticAnnotation
