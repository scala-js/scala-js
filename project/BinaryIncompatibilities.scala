import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
  )

  val Tools = Seq(
      // private[emitter], not an issue
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.tools.linker.backend.emitter.JSGen.this"),

      // private[optimizer], not an issue
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.OptimizerCore#RollbackException.this"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.OptimizerCore#RollbackException.savedStatesInUse"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.OptimizerCore#RollbackException.savedUsedLabelNames"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.OptimizerCore#RollbackException.savedUsedLocalNames"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.OptimizerCore#RollbackException.stateBackups"),
      ProblemFilters.exclude[MissingTypesProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.OptimizerCore$SimpleState"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.OptimizerCore#SimpleState.this"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.OptimizerCore#SimpleState.makeBackup"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.OptimizerCore$State")
  )

  val JSEnvs = Seq(
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
  )

  val TestInterface = Seq(
  )
}
