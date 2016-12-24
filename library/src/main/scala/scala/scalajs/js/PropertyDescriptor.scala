/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package scala.scalajs.js

import scala.scalajs.js

@js.native
trait PropertyDescriptor extends Object {
  var configurable: Boolean = js.native
  var enumerable: Boolean = js.native
  var value: Any = js.native
  var writable: Boolean = js.native
  var get: Function0[Any] = js.native
  var set: Function1[Any, Any] = js.native
}
