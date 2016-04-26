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

  val JSEnvsTestKit = Seq(
  )

  val SbtPlugin = Seq(
  )

  val TestAdapter = Seq(
  )

  val CLI = Seq(
  )

  val Library = Seq(
      /* In theory breaking, but JSStringOps is unlikely to be extended in practice
       * + it's a facade type, so there are less constraints on binary compatibility.
       */
      ProblemFilters.exclude[MissingMethodProblem](
          "scala.scalajs.js.JSStringOps.normalize"),
      ProblemFilters.exclude[MissingMethodProblem](
          "scala.scalajs.js.JSStringOps.normalize$default$1")
  )

  val TestInterface = Seq(
  )
}
