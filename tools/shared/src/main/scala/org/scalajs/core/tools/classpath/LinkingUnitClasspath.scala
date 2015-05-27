/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.classpath

import org.scalajs.core.tools.optimizer.LinkingUnit
import org.scalajs.core.tools.sem.Semantics
import org.scalajs.core.tools.javascript.OutputMode
import org.scalajs.core.tools.io._
import org.scalajs.core.tools.logging._
import org.scalajs.core.tools.optimizer.Emitter
import org.scalajs.core.tools.sourcemap.JSFileBuilder

import scala.collection.immutable.Seq

/** A [[CompleteClasspath]] that is linked as a [[LinkingUnit]].
 *
 *  It does not refer to a single JavaScript file yet.
 */
final class LinkingUnitClasspath(
    jsLibs: Seq[ResolvedJSDependency],
    val linkingUnit: LinkingUnit,
    requiresDOM: Boolean,
    version: Option[String]
) extends CompleteClasspath(jsLibs, requiresDOM, version) {

  /** Emits the linking unit as a single JavaScript file.
   *
   *  Consider using [[ScalaJSOptimizer]] for a canonical way to do so. It
   *  allows to persist the resulting file and create a source map, as well as
   *  using non-default [[Semantics]] and [[OutputMode]].
   *
   *  The [[OutputMode]] is not specified, but it is compliant with
   *  ECMAScript 5.1.
   */
  override lazy val scalaJSCode: VirtualJSFile = {
    val outName = "scalajscode.js"

    val semantics = Semantics.Defaults
    val outputMode = OutputMode.ECMAScript51Isolated

    val output = WritableMemVirtualJSFile(outName)
    val builder = new JSFileBuilder(output.name, output.contentWriter)
    try {
      val emitter = new Emitter(semantics, outputMode)
      emitter.emitAll(linkingUnit, builder, NullLogger)
      builder.complete()
    } finally {
      builder.closeWriters()
    }
    output
  }

}
