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

import scala.scalajs.js

trait PropertyDescriptor extends js.Object {
  // All kinds of property descriptors

  var configurable: js.UndefOr[Boolean] = js.undefined
  var enumerable: js.UndefOr[Boolean] = js.undefined

  // Data descriptors

  var value: js.UndefOr[scala.Any] = js.undefined
  var writable: js.UndefOr[Boolean] = js.undefined

  // Accessor descriptors

  var get: js.UndefOr[js.Function0[scala.Any]] = js.undefined
  var set: js.UndefOr[js.Function1[scala.Any, scala.Any]] = js.undefined
}
