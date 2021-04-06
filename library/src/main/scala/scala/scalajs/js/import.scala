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

/** <span class="badge badge-ecma2020" style="float: right;">ECMAScript 2020</span>
 *  Dynamic `import(specifier)`.
 *
 *  This is an object rather than a simple `def` to reserve the possibility to
 *  add "fields" to the function, notably to support the `import.meta`
 *  meta-property of ECMAScript (still being specified).
 */
object `import` { // scalastyle:ignore
  /** <span class="badge badge-ecma2020" style="float: right;">ECMAScript 2020</span>
   *  Dynamic `import(specifier)`.
   */
  def apply[A <: js.Any](specifier: String): js.Promise[A] =
    throw new java.lang.Error("stub")
}
