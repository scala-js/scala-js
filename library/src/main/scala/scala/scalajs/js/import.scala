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
 *  Dynamic `import(specifier)` and meta-property `import.meta`.
 */
object `import` { // scalastyle:ignore
  /** <span class="badge badge-ecma2020" style="float: right;">ECMAScript 2020</span>
   *  Dynamic `import(specifier)`.
   */
  def apply[A <: js.Any](specifier: String): js.Promise[A] =
    throw new java.lang.Error("stub")

  /** <span class="badge badge-ecma2020" style="float: right;">ECMAScript 2020</span>
   *  Meta-property `import.meta`.
   */
  def meta: js.Dynamic =
    throw new java.lang.Error("stub")
}
