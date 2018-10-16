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

/** Utilities related to global refs mentioned in the program.
 *
 *  When a global ref is mentioned somewhere in the program, bad things happen.
 *
 *  On the one hand, in the local method where it is mentioned, no local
 *  variable can use the same name, as it would shadow the reference to the
 *  global variable. That is however a local property, and is handled by the
 *  name management of [[FunctionEmitter]].
 *
 *  On the other hand, there are so-called "dangerous" global refs, which are
 *  global refs that could collide with the emitter's own internal encoding,
 *  such as `$doubleToInt` or `$c_O`. When a dangerous global ref is mentioned
 *  somewhere in the program, the *entire* emitted program is affected, because
 *  the internal name must be changed.
 *
 *  Dangerous global refs are those that start with `$`, with the exception of
 *  the string `"$"` itself (because jQuery ...).
 *
 *  Hopefully, in a reasonable program, there is actually no dangerous global
 *  ref mentioned anywhere, and the [[Emitter]] can do its job in one pass.
 *  That is why we eagerly filter out non-dangerous global refs in individual
 *  caches, and why we have most paths optimized for empty sets.
 */
private[emitter] object GlobalRefUtils {
  /** Semantically equivalent to `a ++ b`, but optimized for empty sets.
   *
   *  Using this method over `a ++ b` is meaningful is there is a strong
   *  likelihood that one or both parameters are empty, which is the case for
   *  sets of mentioned global refs.
   */
  def unionPreserveEmpty(a: Set[String], b: Set[String]): Set[String] = {
    if (a.isEmpty) b
    else if (b.isEmpty) a
    else a ++ b
  }

  /** Tests whether a global ref is dangerous. */
  def isDangerousGlobalRef(globalRef: String): Boolean = {
    // Note that this intentionally filters out `$` itself
    globalRef.length > 1 && globalRef.charAt(0) == '$'
  }

  /** Filters a set to keep only the dangerous global refs. */
  def keepOnlyDangerousGlobalRefs(allGlobalRefs: Set[String]): Set[String] = {
    if (allGlobalRefs.isEmpty) {
      // Fast path, make sure to return a Set.EmptySet
      Set.empty
    } else {
      // Slow path in a different `def` to keep it out of the JIT's way
      def slowPath(): Set[String] = {
        val r = allGlobalRefs.filter(isDangerousGlobalRef(_))
        /* In the likely empty case, make sure to return a Set.EmptySet,
         * whose `contains()` method is super efficient and can potentially
         * be JIT'ed away.
         */
        if (r.isEmpty)
          Set.empty
        else
          r
      }
      slowPath()
    }
  }
}
