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

package org.scalajs.core.tools.jsdep

import org.scalajs.core.tools.io._

/** A dependency on a native JavaScript library that has been successfully
 *  resolved
 */
final class ResolvedJSDependency(
    val lib: VirtualJSFile,
    val minifiedLib: Option[VirtualJSFile],
    val info: ResolutionInfo)

object ResolvedJSDependency {
  /** Absolute minimum for a [[ResolvedJSDependency]]:
   *
   *  - The library itself
   *  - Its relative name (lib.name)
   */
  def minimal(lib: VirtualJSFile): ResolvedJSDependency = {
    val info = new ResolutionInfo(lib.name, Set.empty, Nil, None, None)
    new ResolvedJSDependency(lib, None, info)
  }
}
