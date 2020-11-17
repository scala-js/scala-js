package build

import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
  )

  val Linker = Seq(
  )

  val LinkerInterface = Seq(
  )

  val SbtPlugin = Seq(
  )

  val TestAdapter = Seq(
      // private[adapter], not an issue
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "org.scalajs.testing.adapter.JSEnvRPC.this"),

      // private[testing], not an issue
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.testing.common.JVMEndpoints.*"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.testing.common.JSEndpoints.*"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.testing.common.FrameworkMessage.*"),
  )

  val Library = Seq(
  )

  val TestInterface = Seq(
  )

  val JUnitRuntime = Seq(
  )
}
