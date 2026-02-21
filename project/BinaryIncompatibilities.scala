package build

import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
    // !!! Breaking, OK in minor release

    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Types#ClassType.this"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Types#ClassType.apply"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Types#ClassType.copy"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Types#ArrayType.this"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Types#ArrayType.apply"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Types#ArrayType.copy"),

    ProblemFilters.exclude[MissingTypesProblem]("org.scalajs.ir.Types$ClassType$"),
    ProblemFilters.exclude[MissingTypesProblem]("org.scalajs.ir.Types$ArrayType$"),
  )

  val Linker = Seq(
    // private, not an issue
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.linker.standard.CoreSpec.this"),
  )

  val LinkerInterface = Seq(
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
