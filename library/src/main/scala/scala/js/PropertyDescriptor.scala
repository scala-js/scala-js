/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.js

trait PropertyDescriptor extends Object {
  var configurable: Boolean = _
  var enumerable: Boolean = _
  var value: Any = _
  var writable: Boolean = _
  var get: Function0[Any] = _
  var set: Function1[Any, Any] = _
}
