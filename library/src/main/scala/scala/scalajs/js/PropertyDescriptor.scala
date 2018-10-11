/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

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
