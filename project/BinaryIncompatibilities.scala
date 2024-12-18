package build

import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
    // !!! Breaking, OK in minor release
    ProblemFilters.exclude[MissingClassProblem]("org.scalajs.ir.Trees$JSLinkingInfo"),
    ProblemFilters.exclude[MissingClassProblem]("org.scalajs.ir.Trees$JSLinkingInfo$"),
    ProblemFilters.exclude[MissingClassProblem]("org.scalajs.ir.Trees$This"),
    ProblemFilters.exclude[MissingTypesProblem]("org.scalajs.ir.Trees$This$"),
    ProblemFilters.exclude[IncompatibleResultTypeProblem]("org.scalajs.ir.Trees#This.apply"),
    ProblemFilters.exclude[IncompatibleMethTypeProblem]("org.scalajs.ir.Trees#This.unapply"),
    ProblemFilters.exclude[IncompatibleResultTypeProblem]("org.scalajs.ir.Trees#*.tpe"),
    ProblemFilters.exclude[MissingClassProblem]("org.scalajs.ir.Types$NoType$"),
  )

  val Linker = Seq(
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
