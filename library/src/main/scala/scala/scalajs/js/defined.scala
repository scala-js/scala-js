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

object defined {
  /** Explicitly upcasts an `A` to a `js.UndefOr[A]`.
   *
   *  This method is useful in some cases to drive Scala's type inference.
   *  For example, when calling a method expecting a `js.UndefOr[js.FunctionN]`
   *  as shown below:
   *
   *  {{{
   *  def foo(f: js.UndefOr[js.Function1[Int, Int]] = js.undefined): Int = ???
   *
   *  foo((x: Int) => x + 1) // compile error (requires 2 chained implicits)
   *  foo(js.defined((x: Int) => x + 1)) // compiles
   *
   *  // 2.12+ only:
   *  foo(js.defined(x => x + 1)) // compiles as well
   *  }}}
   */
  def apply[A](a: A): js.UndefOr[A] = a
}
