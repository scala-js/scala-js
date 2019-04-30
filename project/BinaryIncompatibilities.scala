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
      // private, not an issue
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testcommon.RPCCore$JDKCollectionConvertersCompat$*")
  )

  val TestAdapter = TestCommon ++ Seq(
  )

  val CLI = Seq(
  )

  val Library = Seq(
      // private[niocharset], not an issue
      ProblemFilters.exclude[MissingClassProblem]("scala.scalajs.niocharset.ISO_*"),
      ProblemFilters.exclude[MissingClassProblem]("scala.scalajs.niocharset.US_*"),
      ProblemFilters.exclude[MissingClassProblem]("scala.scalajs.niocharset.UTF_*")
  )

  val TestInterface = TestCommon ++ Seq(
  )
}
