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

/** Amount of global ref tracking that we need to do in a given context.
 *
 *  - When using GCC, we always track all global refs;
 *  - Otherwise, inside the body of a function, in `FunctionEmitter`, we track
 *    all global refs so that we can identify collisions with locally allocated
 *    variable names;
 *  - Otherwise, we only track dangerous global refs.
 */
private[emitter] sealed abstract class GlobalRefTracking {
  import GlobalRefTracking._

  def shouldTrack(globalRef: String): Boolean = this match {
    case All       => true
    case Dangerous => GlobalRefUtils.isDangerousGlobalRef(globalRef)
  }

  /** Given a set of global refs tracked under the rules of `fromTracking`,
   *  keep only the ones needed according to `this`.
   */
  def refineFrom(fromTracking: GlobalRefTracking, globalRefs: Set[String]): Set[String] = {
    if (this == fromTracking)
      globalRefs
    else if (this == Dangerous)
      GlobalRefUtils.keepOnlyDangerousGlobalRefs(globalRefs)
    else
      throw new AssertionError(s"Cannot refine set of global refs from $fromTracking to $this")
  }
}

private[emitter] object GlobalRefTracking {
  case object All extends GlobalRefTracking
  case object Dangerous extends GlobalRefTracking
}
