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
  def linkNode(cpEntries: String*): String =
    linkNodeInternal(Semantics.Defaults, cpEntries: _*)

  /** Link the Scala.js test suite on Node.js */
  @JSExport
  def linkTestSuiteNode(cpEntries: String*): String = {
    val semantics = Semantics.Defaults.withRuntimeClassName(_.fullName match {
      case "org.scalajs.testsuite.compiler.ReflectionTest$RenamedTestClass" =>
        "renamed.test.Class"
      case fullName =>
        fullName
    })
    linkNodeInternal(semantics, cpEntries: _*)
  }

  /** Link a Scala.js application on Node.js */
  def linkNodeInternal(semantics: Semantics, cpEntries: String*): String = {
    val builder = new AbstractPartialClasspathBuilder with NodeFileSystem
    val cp = builder.build(cpEntries.toList)

    val complete = cp.resolve()

    val optimizer = new ScalaJSOptimizer(semantics.optimized)

    val out = WritableMemVirtualJSFile("out.js")

    import ScalaJSOptimizer._
    val optimized = optimizer.optimizeCP(complete, Config(out),
        new ScalaConsoleLogger)

    out.content
  }

}
