import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
  )

  val Tools = Seq(
      // private, not an issue
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.OptimizerCore#RollbackException.this")
  )

  val JSEnvs = Seq(
  )

  val SbtPlugin = Seq(
  )

  val TestAdapter = Seq(
  )

  val CLI = Seq(
  )

  val Library = Seq(
      // private[concurrent], not an issue
      ProblemFilters.exclude[MissingTypesProblem](
          "scala.scalajs.concurrent.QueueExecutionContext$"),
      ProblemFilters.exclude[MissingMethodProblem](
          "scala.scalajs.concurrent.QueueExecutionContext.reportFailure"),
      ProblemFilters.exclude[MissingMethodProblem](
          "scala.scalajs.concurrent.QueueExecutionContext.execute"),
      ProblemFilters.exclude[MissingMethodProblem](
          "scala.scalajs.concurrent.QueueExecutionContext.prepare")
  )

  val TestInterface = Seq(
    )
}
