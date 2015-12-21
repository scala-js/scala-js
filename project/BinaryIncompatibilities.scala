import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
  )

  val Tools = Seq(
  )

  val JSEnvs = Seq(
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.jsenv.nodejs.NodeJSEnv$AbstractNodeRunner"),
      ProblemFilters.exclude[MissingTypesProblem](
          "org.scalajs.jsenv.nodejs.NodeJSEnv$NodeRunner"),
      ProblemFilters.exclude[MissingTypesProblem](
          "org.scalajs.jsenv.nodejs.NodeJSEnv$AsyncNodeRunner"),
      ProblemFilters.exclude[MissingTypesProblem](
          "org.scalajs.jsenv.nodejs.NodeJSEnv$ComNodeRunner")
  )

  val SbtPlugin = Seq(
  )

  val TestAdapter = Seq(
  )

  val CLI = Seq(
  )

  val Library = Seq(
  )

  val TestInterface = Seq(
  )
}
