package build

import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
  )

  val Tools = Seq(
  )

  val JSEnvs = Seq(
  )

  val JSEnvsTestKit = Seq(
  )

  val SbtPlugin = Seq(
  )

  val TestCommon = Seq(
      // private[scalajs], not an issue.
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.testcommon.RPCCore.call"),
      ProblemFilters.exclude[ReversedMissingMethodProblem](
          "org.scalajs.testcommon.RPCCore.toFuture")
  )

  val TestAdapter = TestCommon ++ Seq(
      // private, not an issue.
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testadapter.ScalaJSRunner$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testadapter.ScalaJSRunner$RichFuture"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testadapter.ScalaJSRunner$RichFuture$")
  )

  val CLI = Seq(
  )

  val Library = Seq(
  )

  val TestInterface = TestCommon ++ Seq(
  )
}
