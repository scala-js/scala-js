/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.classpath

import scala.collection.immutable.{Seq, Traversable}

import org.scalajs.core.tools.sem.Semantics
import org.scalajs.core.tools.javascript.OutputMode
import org.scalajs.core.tools.io._
import org.scalajs.core.tools.logging._
import org.scalajs.core.tools.optimizer.ScalaJSOptimizer
import org.scalajs.core.tools.jsdep.ResolutionInfo

/** A [[CompleteClasspath]] that contains only IR as scalaJSCode */
final class IRClasspath(
    /** The JS libraries the IR code depends on */
    jsLibs: Seq[ResolvedJSDependency],
    val requiredCompliance: Traversable[ComplianceRequirement],
    /** The IR itself. Ancestor count is used for later ordering */
    val scalaJSIR: Traversable[VirtualScalaJSIRFile],
    requiresDOM: Boolean,
    version: Option[String]
) extends CompleteClasspath(jsLibs, requiresDOM, version) {

  /** Orders and optimizes the contained IR.
   *
   *  Consider using [[optimizer.ScalaJSOptimizer ScalaJSOptimizer]] for a
   *  canonical way to do so. It allows to persist the resulting file and create
   *  a source map, as well as using non-default [[sem.Semantics Semantics]] and
   *  [[javascript.OutputMode OutputMode]].
   *
   *  The [[javascript.OutputMode OutputMode]] is not specified, but it is
   *  compliant with ECMAScript 5.1.
   */
  override lazy val scalaJSCode: VirtualJSFile = {
    import ScalaJSOptimizer._

    val outName = "temporary-fastOpt.js"

    if (scalaJSIR.nonEmpty) {
      val semantics = Semantics.compliantTo(requiredCompliance.map(_.semantics))
      val output = WritableMemVirtualJSFile(outName)
      new ScalaJSOptimizer(semantics, OutputMode.ECMAScript51Isolated).optimizeCP(
          this, Config(output), NullLogger)
      output
    } else {
      // We cannot run the optimizer without IR, because it will complain about
      // java.lang.Object missing. However, an empty JS file is perfectly valid
      // for no IR at all.
      VirtualJSFile.empty(outName)
    }
  }

  /** Checks whether the given semantics are compliant with the requirements of
   *  this CompleteClasspath. Throws an exception otherwise.
   */
  final def checkCompliance(semantics: Semantics): Unit = {
    val unmet = requiredCompliance filterNot { compliance =>
      semantics.isCompliant(compliance.semantics)
    }

    if (unmet.nonEmpty)
      throw new BadComplianceException(unmet.toList)
  }
}
