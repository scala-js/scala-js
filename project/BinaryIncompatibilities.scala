package build

import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
    // !!! Breaking, OK in minor release
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Trees#StoreModule.*"),
    ProblemFilters.exclude[IncompatibleResultTypeProblem]("org.scalajs.ir.Trees#StoreModule.unapply"),
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
