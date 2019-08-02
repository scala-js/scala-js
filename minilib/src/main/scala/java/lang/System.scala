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

package java.lang

/** Stripped down version of `java.lang.System` with the bare minimum to
 *  support a correct `Object.hashCode()`.
 *
 *  We cannot use the full `java.lang.System` out of the box, because its
 *  constructor initializes `out` and `err`, which require
 *  `JSConsoleBasedPrintStream` and therefore a large part of `java.io`. We do
 *  not simply reuse (copy-paste) `identityHashCode` either because it
 *  internally uses a `WeakMap` typed as `js.Dynamic`.
 */
object System {
  // Simpler than the original, technically valid but of lesser quality
  def identityHashCode(x: Object): scala.Int = {
    (x: Any) match {
      case null => 0
      case _:scala.Boolean | _:scala.Double | _:String | () =>
        x.hashCode()
      case _ =>
        // See the original System.scala for the rationale of this test
        if (x.getClass == null)
          x.hashCode()
        else
          42
    }
  }
}
