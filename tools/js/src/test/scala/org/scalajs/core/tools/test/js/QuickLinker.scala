package org.scalajs.core.tools.test.js

import org.scalajs.core.tools.sem.Semantics
import org.scalajs.core.tools.io._
import org.scalajs.core.tools.logging._
import org.scalajs.core.tools.classpath._
import org.scalajs.core.tools.classpath.builder._
import org.scalajs.core.tools.optimizer._

import scala.scalajs.js.annotation.JSExport

@JSExport("scalajs.QuickLinker")
object QuickLinker {

  /** Link a Scala.js application on Node.js */
  @JSExport
  def linkNode(cpEntries: String*): String = {
    val builder = new AbstractPartialClasspathBuilder with NodeFileSystem
    val cp = builder.build(cpEntries.toList)

    val complete = cp.resolve()

    val optimizer = new ScalaJSOptimizer(Semantics.Defaults.optimized)

    val out = WritableMemVirtualJSFile("out.js")

    import ScalaJSOptimizer._
    val optimized = optimizer.optimizeCP(
      Inputs(complete),
      OutputConfig(out),
      new ScalaConsoleLogger
    )

    out.content
  }

}
