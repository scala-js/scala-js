package build

import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
    // !!! Breaking, OK in minor release

    ProblemFilters.exclude[MissingTypesProblem]("org.scalajs.ir.Names$FieldName"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Names#FieldName.*"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Names#LocalName.fromFieldName"),

    ProblemFilters.exclude[IncompatibleMethTypeProblem]("org.scalajs.ir.Types#RecordType.findField"),
    ProblemFilters.exclude[MemberProblem]("org.scalajs.ir.Types#RecordType#Field.*"),

    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Trees#StoreModule.*"),
    ProblemFilters.exclude[IncompatibleResultTypeProblem]("org.scalajs.ir.Trees#StoreModule.unapply"),

    ProblemFilters.exclude[MemberProblem]("org.scalajs.ir.Trees#JSPrivateSelect.*"),
    ProblemFilters.exclude[MemberProblem]("org.scalajs.ir.Trees#RecordSelect.*"),
    ProblemFilters.exclude[MemberProblem]("org.scalajs.ir.Trees#Select.*"),
    ProblemFilters.exclude[MemberProblem]("org.scalajs.ir.Trees#SelectStatic.*"),
  )

  val Linker = Seq(
    // private, not an issue
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.linker.standard.CommonPhaseConfig.this"),
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
