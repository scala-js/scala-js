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

import scala.language.implicitConversions

import scala.scalajs.js

/** Provides implicit conversions and operations to write in JavaScript
 *  style with [[js.Dynamic]].
 *
 *  Be **very** careful when importing members of this object. You may want
 *  to selectively import the implicits that you want to reduce the likelihood
 *  of making mistakes.
 */
object DynamicImplicits {
  @inline implicit def truthValue(x: js.Dynamic): Boolean =
    (!(!x)).asInstanceOf[Boolean]

  implicit def number2dynamic(x: Double): js.Dynamic =
    x.asInstanceOf[js.Dynamic]

  implicit def boolean2dynamic(x: Boolean): js.Dynamic =
    x.asInstanceOf[js.Dynamic]
}
