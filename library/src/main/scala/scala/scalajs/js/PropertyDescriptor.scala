/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */



package scala.scalajs.js

@native
trait PropertyDescriptor extends Object {
  var configurable: Boolean = native
  var enumerable: Boolean = native
  var value: Any = native
  var writable: Boolean = native
  var get: Function0[Any] = native
  var set: Function1[Any, Any] = native
}
