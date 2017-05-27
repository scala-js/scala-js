import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
  )

  val Tools = Seq(
      // private, not an issue
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.tools.linker.Linker#Config.this"),

      // private, not an issue
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.tools.linker.frontend.LinkerFrontend#Config.this"),

      // private, not an issue
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.tools.linker.backend.LinkerBackend#Config.this"),

      // private[optimizer], not an issue
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.GenIncOptimizer.this")
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
