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
import scala.scalajs.js.annotation._

/** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
 *  JavaScript Generator.
 */
trait Generator[+Elem, +Result, -NextParam] extends js.Iterator[Elem] with js.Iterable[Elem] {
  def next(): js.Generator.Entry[Elem, Result]
  def next(param: NextParam): js.Generator.Entry[Elem, Result]
  def `return`[R >: Result](value: R): js.Generator.Entry[Elem, R]
  def `throw`(exception: scala.Any): js.Generator.Entry[Elem, Result]

  @JSName(js.Symbol.iterator)
  def jsIterator(): this.type
}

object Generator {
  def apply[Elem, Result, NextParam](
      body: YieldEvidence[Elem, NextParam] => Result): js.Generator[Elem, Result, NextParam] = {
    throw new java.lang.Error("stub")
  }

  def `yield`[Elem, NextParam](value: Elem)(
      implicit evidence: YieldEvidence[Elem, NextParam]): NextParam = {
    throw new java.lang.Error("stub")
  }

  def yield_*[Elem, NextParam](values: js.Iterable[Elem])(
      implicit evidence: YieldEvidence[Elem, NextParam]): NextParam = {
    throw new java.lang.Error("stub")
  }

  /** Return value of `js.Generator.next`. */
  trait Entry[+Elem, +Result] extends js.Iterator.Entry[Elem] {
    /** The result value. Reading this value is only valid if done is true. */
    @JSName("value")
    def resultValue: Result
  }

  sealed trait YieldEvidence[Elem, NextParam] extends js.Any
}
