import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
  )

  val Tools = Seq(
      // Private things, not an issue
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.optimizer.OptimizerCore.org$scalajs$core$tools$optimizer$OptimizerCore$$shouldInlineBecauseOfArgs"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.optimizer.OptimizerCore.org$scalajs$core$tools$optimizer$OptimizerCore$$treeNotInlined0$2"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.optimizer.OptimizerCore.org$scalajs$core$tools$optimizer$OptimizerCore$$treeNotInlined0$3"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "org.scalajs.core.tools.optimizer.OptimizerCore.org$scalajs$core$tools$optimizer$OptimizerCore$$inline")
  )

  val JSEnvs = Seq(
      // Private things, not an issue
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.jsenv.rhino.ScalaJSCoreLib.org$scalajs$jsenv$rhino$ScalaJSCoreLib$$loadAncestors")
  )

  val SbtPlugin = Seq(
  )

  val TestAdapter = Seq(
  )
}
