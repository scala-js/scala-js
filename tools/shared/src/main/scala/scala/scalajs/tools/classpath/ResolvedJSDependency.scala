package scala.scalajs.tools.classpath

import scala.scalajs.tools.io._
import scala.scalajs.tools.jsdep._

/** A dependency on a native JavaScript library that has been successfully
 *  resolved
 */
final class ResolvedJSDependency(
    val lib: VirtualJSFile, val info: ResolutionInfo)
