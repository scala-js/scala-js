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
  )

  val TestInterface = Seq(
      // Private things, not an issue
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "sbt.testing.Status.this"),

      // Inherited from parent class, not an issue
      ProblemFilters.exclude[MissingTypesProblem](
          "sbt.testing.Status"),
      ProblemFilters.exclude[MissingMethodProblem](
          "sbt.testing.Status.name"),
      ProblemFilters.exclude[MissingMethodProblem](
          "sbt.testing.Status.ordinal"),
      ProblemFilters.exclude[MissingMethodProblem](
          "sbt.testing.Status.toString")
    )
}
