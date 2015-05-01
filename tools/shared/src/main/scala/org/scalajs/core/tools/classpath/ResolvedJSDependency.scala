package org.scalajs.core.tools.classpath

import org.scalajs.core.tools.io._
import org.scalajs.core.tools.jsdep._

/** A dependency on a native JavaScript library that has been successfully
 *  resolved
 */
final class ResolvedJSDependency(
    val lib: VirtualJSFile,
    val minifiedLib: Option[VirtualJSFile],
    val info: ResolutionInfo) {

  @deprecated("Use the version with minifiedLib instead", "0.6.3")
  def this(lib: VirtualJSFile, info: ResolutionInfo) =
    this(lib, None, info)

}
