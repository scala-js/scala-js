import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
  )

  val Tools = Seq(
      // New method in a trait sealed via self-type, not an issue
      ProblemFilters.exclude[ReversedMissingMethodProblem](
          "org.scalajs.core.tools.linker.LinkerPlatformExtensions.applyInternal"),

      // private[optimizer], not an issue
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.GenIncOptimizer#MethodContainer.optimizedDefs"),

      // private[emitter], not an issue
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.tools.linker.backend.emitter.FunctionEmitter#JSDesugar.genClassDataOf")
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
