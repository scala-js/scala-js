package build

import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
    // private, not an issue
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Serializers#Hacks.*"),
  )

  val Linker = Seq(
    // Breaking (minor change)
    exclude[DirectMissingMethodProblem]("org.scalajs.linker.standard.LinkedClass.this"),
    // private[linker], not an issue.
    exclude[DirectMissingMethodProblem]("org.scalajs.linker.standard.LinkedClass.refined"),
  )

  val LinkerInterface = Seq(
    // private, not an issue
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.linker.interface.Semantics.this"),
  )

  val SbtPlugin = Seq(
  )

  val TestAdapter = Seq(
  )

  val Library = Seq(
    // Static initializer (2.11 only), not an issue
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.scalajs.js.JavaScriptException.<clinit>"),
  )

  val TestInterface = Seq(
  )

  val JUnitRuntime = Seq(
  )
}
