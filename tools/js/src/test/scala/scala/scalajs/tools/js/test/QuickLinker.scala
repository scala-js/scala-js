package scala.scalajs.tools.js.test

import scala.scalajs.tools.sem.Semantics
import scala.scalajs.tools.io._
import scala.scalajs.tools.logging._
import scala.scalajs.tools.classpath._
import scala.scalajs.tools.classpath.builder._
import scala.scalajs.tools.optimizer._

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
