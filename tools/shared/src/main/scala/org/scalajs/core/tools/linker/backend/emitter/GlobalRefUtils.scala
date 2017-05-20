/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2017, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package org.scalajs.core.tools.linker.backend.emitter

import org.scalajs.core.tools.linker.backend.OutputMode

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
 *
 *  Note that when using the ECMAScript51Global output mode, internal encoding
 *  variables are properties of the global `ScalaJS` variable, and therefore
 *  never need to be renamed. This is good news too, as Rhino desugars classes
 *  one at a time, so we would never know whether a global ref was actually
 *  used or not at the time we emit one class.
 *
 *  The naive reader might thing that `ScalaJS` *itself* would need to be
 *  renamed if it was mentioned as global ref. But in fact, a piece of JS code
 *  referencing `ScalaJS` would actually see that Scala.js implementation
 *  detail, so Scala.js code referencing it should also see it!
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

  /** Tests whether a global ref is dangerous.
   *
   *  This method assumes that the output mode is not `ECMAScript51Global`. In
   *  that mode, no global ref is ever dangerous and this method is irrelevant.
   */
  private def isDangerousGlobalRef(globalRef: String): Boolean = {
    // Note that this intentionally filters out `$` itself
    globalRef.length > 1 && globalRef.charAt(0) == '$'
  }

  /** Filters a set to keep only the dangerous global refs.
   *
   *  If the output mode is `ECMAScript51Global`, this method always returns
   *  an empty set.
   */
  def keepOnlyDangerousGlobalRefs(allGlobalRefs: Set[String])(
      implicit outputMode: OutputMode): Set[String] = {
    if (allGlobalRefs.isEmpty) {
      // Fast path
      allGlobalRefs
    } else {
      def slowPath(): Set[String] = {
        if (outputMode == OutputMode.ECMAScript51Global) {
          Set.empty
        } else {
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
      }
      slowPath()
    }
  }
}
