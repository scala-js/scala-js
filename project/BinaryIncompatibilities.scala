import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
  )

  val Tools = Seq(
  )

  val JSEnvs = Seq(
  )

  val SbtPlugin = Seq(
  )

  val TestAdapter = Seq(
  )

  val CLI = Seq(
      // Private things, not an issue
      ProblemFilters.exclude[MissingTypesProblem](
          "org.scalajs.cli.Scalajsp$Options$"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.cli.Scalajsp#Options.apply$default$5"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsp#Options.apply$default$4"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.cli.Scalajsp#Options.<init>$default$5"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsp#Options.apply$default$3"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.cli.Scalajsp#Options.apply"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.cli.Scalajsp#Options.apply"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsp#Options.<init>$default$4"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsp#Options.<init>$default$3"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.cli.Scalajsp#Options.desugar"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.cli.Scalajsp#Options.copy$default$5"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.cli.Scalajsp#Options.copy"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsp#Options.copy$default$4"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsp#Options.copy$default$3"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.cli.Scalajsp#Options.this")
  )
}
