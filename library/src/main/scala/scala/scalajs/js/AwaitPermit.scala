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

/** Kind of permit for calling `js.await()`.
 *
 *  A default permit is always available. It allows to call `js.await()` when
 *  directly enclosed by a `js.async { ... }` block.
 *
 *  @see [[scala.scalajs.js.async]]
 */
abstract class AwaitPermit private[js] ()

object AwaitPermit {
  /** Default permit for `js.await()` calls directly enclosed within a
   *  `js.async` block.
   */
  implicit object directlyWithinAsync extends AwaitPermit
}
