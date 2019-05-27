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
  )

  val TestAdapter = TestCommon ++ Seq(
  )

  val CLI = Seq(
  )

  val Library = Seq(
  )

  val TestInterface = Seq(
    /* Things that moved to scalajs-test-bridge.
     * They were private[scalajs] or stricter, so not an issue.
     */
    ProblemFilters.exclude[MissingClassProblem]("org.scalajs.testcommon.*"),
    ProblemFilters.exclude[MissingClassProblem]("org.scalajs.testinterface.HTMLRunner"),
    ProblemFilters.exclude[MissingClassProblem]("org.scalajs.testinterface.HTMLRunner$*"),
    ProblemFilters.exclude[MissingClassProblem]("org.scalajs.testinterface.TestDetector"),
    ProblemFilters.exclude[MissingClassProblem]("org.scalajs.testinterface.TestDetector$*"),
    ProblemFilters.exclude[MissingClassProblem]("org.scalajs.testinterface.internal.*")
  )
}
