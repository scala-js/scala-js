package build

import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
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
    // New abstract member in JS trait, not an issue
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.scalajs.runtime.LinkingInfo.isWebAssembly"),
  )

  val TestInterface = Seq(
  )

  val JUnitRuntime = Seq(
  )
}
