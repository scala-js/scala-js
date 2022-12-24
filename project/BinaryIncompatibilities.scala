package build

import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
    // Breaking, but in minor verison, so OK.
    exclude[DirectMissingMethodProblem]("org.scalajs.ir.Trees#JSPropertyDef.copy"),
    exclude[DirectMissingMethodProblem]("org.scalajs.ir.Trees#JSPropertyDef.this"),
    exclude[DirectMissingMethodProblem]("org.scalajs.ir.Trees#JSPropertyDef.apply"),
    exclude[MissingClassProblem]("org.scalajs.ir.Trees$DoWhile"),
    exclude[MissingClassProblem]("org.scalajs.ir.Trees$DoWhile$"),
  )

  val Linker = Seq(
    // private[linker], not an issue
    exclude[DirectMissingMethodProblem]("org.scalajs.linker.standard.LinkedClass.optimized"),
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
  )

  val TestInterface = Seq(
  )

  val JUnitRuntime = Seq(
  )
}
