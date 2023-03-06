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

package org.scalajs.linker.backend.emitter

import GlobalRefUtils.unionPreserveEmpty

/** A monad that associates a set of global variable names to a value.
 *
 *  This is used to track the set of (dangerous) global variable names used in
 *  a tree (or a list of trees), and easily perform operations that compose
 *  such trees while accumulating all the mentioned global variable names.
 *
 *  Below follows a proof that `WithGlobals` is indeed a monad. We do not
 *  actually use that property anywhere. The proof is there only so that I can
 *  call it a monad and not be attacked by an army of cats.
 *
 *  Definition:
 *  {{{
 *  WithGlobals.apply(a) === WithGlobals(a, ∅)
 *
 *  WithGlobals(v, w).flatMap(f) === WithGlobals(f(v).value, w ∪ f(v).globalVarNames)
 *  }}}
 *
 *  Left identity:
 *  {{{
 *               WithGlobals.apply(a).flatMap(f)     =?= f(a)
 *               --------------------
 *           WithGlobals(a, ∅).flatMap(f)            =?= f(a)
 *           ----------------------------
 *  WithGlobals(f(a).value, ∅ ∪ f(a).globalVarNames) =?= f(a)
 *                          -----------------------
 *  WithGlobals(f(a).value,   f(a).globalVarNames  ) =?= f(a)
 *  }}}
 *
 *  Right identity:
 *  {{{
 *  WithGlobals(v, w).flatMap(WithGlobals.apply)                                     =?= WithGlobals(v, w)
 *  --------------------------------------------
 *  WithGlobals(WithGlobals.apply(v).value, w ∪ WithGlobals.apply(v).globalVarNames) =?= WithGlobals(v, w)
 *              --------------------------      -----------------------------------
 *  WithGlobals(          v               , w ∪                 ∅                    =?= WithGlobals(v, w)
 *  }}}
 *
 *  Associativity:
 *  {{{
 *  WithGlobals(v, w).flatMap(f).flatMap(g) =?= WithGlobals(v, w).flatMap(x => f(x).flatMap(g))
 *  ------------
 *  WithGlobals(f(v).value, w ∪ f(v).globalVarNames).flatMap(g) =?= ...
 *  -----------------------------------------------------------
 *  WithGlobals(g(f(v).value).value, (w ∪ f(v).globalVarNames) ∪ g(f(v).value).globalVarNames) =?= ...
 *
 *  ... =?= WithGlobals(v, w).flatMap(x => f(x).flatMap(g))
 *          -----------------------------------------------
 *  ... =?= WithGlobals((x => f(x).flatMap(g))(v).value, w ∪ (x => f(x).flatMap(g))(v).globalVarNames)
 *                      -------------------------------      ----------------------------------------
 *
 *  Aside:
 *    (x => f(x).flatMap(g))(v)
 *    === f(v).flatMap(g)
 *    === WithGlobals(g(f(v).value).value, f(v).globalVarNames ∪ g(f(v).value).globalVarNames)
 *
 *  ... =?= WithGlobals(g(f(v).value).value, w ∪ (f(v).globalVarNames ∪ g(f(v).value).globalVarNames))
 *  }}}
 */
private[emitter] final case class WithGlobals[+A](
    value: A, globalVarNames: Set[String]) {

  import WithGlobals._

  def map[B](f: A => B): WithGlobals[B] =
    WithGlobals(f(value), globalVarNames)

  def flatMap[B](f: A => WithGlobals[B]): WithGlobals[B] = {
    val t = f(value)
    WithGlobals(t.value, unionPreserveEmpty(globalVarNames, t.globalVarNames))
  }
}

private[emitter] object WithGlobals {
  /** Constructs a `WithGlobals` with an empty set `globalVarNames`. */
  def apply[A](value: A): WithGlobals[A] =
    new WithGlobals(value, Set.empty)

  val nil: WithGlobals[Nil.type] = WithGlobals(Nil)

  def list[A](xs: List[WithGlobals[A]]): WithGlobals[List[A]] = {
    /* This could be a cascade of flatMap's, but the following should be more
     * efficient.
     */
    val values = xs.map(_.value)
    val globalVarNames = xs.foldLeft(Set.empty[String]) { (prev, x) =>
      unionPreserveEmpty(prev, x.globalVarNames)
    }
    WithGlobals(values, globalVarNames)
  }

  def option[A](xs: Option[WithGlobals[A]]): WithGlobals[Option[A]] =
    xs.fold[WithGlobals[Option[A]]](WithGlobals(None))(_.map(Some(_)))
}
