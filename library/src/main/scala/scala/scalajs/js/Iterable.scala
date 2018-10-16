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
import js.annotation._

/** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
 *  JavaScript Iterable.
 */
trait Iterable[+A] extends js.Object {
  /** JavaScript Iterator for this Iterable. */
  @JSName(js.Symbol.iterator)
  def jsIterator(): js.Iterator[A]
}
