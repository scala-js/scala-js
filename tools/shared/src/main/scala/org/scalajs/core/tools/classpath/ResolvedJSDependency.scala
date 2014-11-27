package org.scalajs.core.tools.classpath

import org.scalajs.core.tools.io._
import org.scalajs.core.tools.jsdep._

/** A dependency on a native JavaScript library that has been successfully
 *  resolved
 */
final class ResolvedJSDependency(
    val lib: VirtualJSFile, val info: ResolutionInfo)
