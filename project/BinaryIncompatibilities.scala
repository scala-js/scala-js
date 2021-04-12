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

  val TestCommon = Seq(
  )

  val TestAdapter = TestCommon ++ Seq(
  )

  val Library = Seq(
    // New method in a sealed trait (even a JS trait), not an issue
    ProblemFilters.exclude[ReversedMissingMethodProblem](
        "scala.scalajs.runtime.LinkingInfo.esVersion"),
  )

  val TestInterface = Seq(
  )
}
