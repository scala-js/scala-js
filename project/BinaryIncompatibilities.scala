package build

import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
    // !!! Breaking, OK in minor release

    ProblemFilters.exclude[IncompatibleResultTypeProblem]("org.scalajs.ir.Trees#*.tpe"),

    ProblemFilters.exclude[IncompatibleMethTypeProblem]("org.scalajs.ir.Trees#NewArray.this"),
    ProblemFilters.exclude[IncompatibleMethTypeProblem]("org.scalajs.ir.Trees#NewArray.apply"),
    ProblemFilters.exclude[IncompatibleMethTypeProblem]("org.scalajs.ir.Trees#NewArray.copy"),
    ProblemFilters.exclude[IncompatibleResultTypeProblem]("org.scalajs.ir.Trees#NewArray.copy$default$2"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Trees#NewArray.lengths"),

    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Trees#UnaryOp.resultTypeOf"),

    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Types#ClassType.this"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Types#ClassType.apply"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Types#ClassType.copy"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Types#ArrayType.this"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Types#ArrayType.apply"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Types#ArrayType.copy"),

    ProblemFilters.exclude[MissingTypesProblem]("org.scalajs.ir.Types$ClassType$"),
    ProblemFilters.exclude[MissingTypesProblem]("org.scalajs.ir.Types$ArrayType$"),

    // New abstract member in sealed hierarchy, not an issue
    ProblemFilters.exclude[ReversedMissingMethodProblem]("org.scalajs.ir.Types#Type.toNonNullable"),
  )

  val Linker = Seq(
    // private, not an issue
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.linker.standard.CoreSpec.this"),

    // private[linker], not an issue
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.linker.standard.CoreSpec.apply"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.linker.standard.ModuleSet.this"),
  )

  val LinkerInterface = Seq(
  )

  val SbtPlugin = Seq(
  )

  val TestAdapter = Seq(
  )

  val Library = Seq(
    // New abstract member in JS trait, not an issue
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.scalajs.runtime.LinkingInfo.isWebAssembly"),
  )

  val TestInterface = Seq(
  )

  val JUnitRuntime = Seq(
  )
}
