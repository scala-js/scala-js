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

/** A monad that associates additional information to a value.
 *
 *  This is used to track the set of (dangerous) global variable names used in
 *  a tree (or a list of trees), and easily perform operations that compose
 *  such trees while accumulating all the mentioned global variable names.
 *
 *  A proof that `WithInfo` is indeed a monad is omitted for brievity but
 *  available for its predecessor `WithGlobals` in the version history. We do
 *  not actually use that property anywhere. The proof is there only so that I
 *  can call it a monad and not be attacked by an army of cats. We leave
 *  adapting said proof to `WithInfo` as an exercise to the reader.
 */
private[emitter] final case class WithInfo[+A](
    value: A, globalVarNames: Set[String]) {

  import WithInfo._

  def map[B](f: A => B): WithInfo[B] =
    WithInfo(f(value), globalVarNames)

  def flatMap[B](f: A => WithInfo[B]): WithInfo[B] = {
    val t = f(value)
    WithInfo(t.value, unionPreserveEmpty(globalVarNames, t.globalVarNames))
  }
}

private[emitter] object WithInfo {
  /** Constructs a `WithInfo` with an empty set `globalVarNames`. */
  def apply[A](value: A): WithInfo[A] =
    new WithInfo(value, Set.empty)

  def list[A](xs: List[WithInfo[A]]): WithInfo[List[A]] = {
    /* This could be a cascade of flatMap's, but the following should be more
     * efficient.
     */
    val values = xs.map(_.value)
    val globalVarNames = xs.foldLeft(Set.empty[String]) { (prev, x) =>
      unionPreserveEmpty(prev, x.globalVarNames)
    }
    WithInfo(values, globalVarNames)
  }

  def option[A](xs: Option[WithInfo[A]]): WithInfo[Option[A]] =
    xs.fold[WithInfo[Option[A]]](WithInfo(None))(_.map(Some(_)))
}
