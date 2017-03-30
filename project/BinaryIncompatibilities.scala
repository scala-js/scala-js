import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
  )

  val Tools = Seq(
      // private[optimizer], not an issue
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.ConcurrencyUtils#AtomicAcc.apply"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.ConcurrencyUtils#AtomicAccOps.removeAll$extension"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.ConcurrencyUtils#AtomicAccOps.removeAll"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.ParIncOptimizer#CollOps.prepAdd"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.ParIncOptimizer#CollOps.finishAdd"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.ParIncOptimizer#CollOps.emptyParIterable")
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
