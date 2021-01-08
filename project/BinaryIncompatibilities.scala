package build

import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
      // Breaking in stable API. OK in Minor version.
      exclude[Problem]("org.scalajs.ir.*"),
  )

  val Linker = Seq(
      // Breaking in stable API. OK in Minor version.
      exclude[Problem]("org.scalajs.linker.standard.*"),

      // New method in sealed trait, not an issue.
      exclude[ReversedMissingMethodProblem](
          "org.scalajs.linker.MemOutputDirectory.fileNames"),
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
      // New concrete method in native JS trait, not an issue.
      exclude[ReversedMissingMethodProblem]("scala.scalajs.js.typedarray.TypedArray.fill"),

      // New optional member in JS trait, not an issue.
      exclude[ReversedMissingMethodProblem]("scala.scalajs.js.RegExp#ExecResult.groups"),
      exclude[ReversedMissingMethodProblem]("scala.scalajs.js.RegExp#ExecResult.groups_="),
  )

  val TestInterface = Seq(
  )
}
