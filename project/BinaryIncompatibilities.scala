package build

import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
  )

  val Tools = Seq(
      // private[closure], not an issue
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.closure.LoggerErrorManager")
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

  val TestInterface = TestCommon ++ Seq(
  )
}
