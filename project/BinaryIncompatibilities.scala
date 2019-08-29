package build

import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
  )

  val Tools = Seq(
      // private[optimizer], not an issue
      ProblemFilters.exclude[IncompatibleMethTypeProblem]("org.scalajs.core.tools.linker.frontend.optimizer.GenIncOptimizer#Class.subclasses_="),
      ProblemFilters.exclude[IncompatibleResultTypeProblem]("org.scalajs.core.tools.linker.frontend.optimizer.GenIncOptimizer#Class.subclasses"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem]("org.scalajs.core.tools.linker.frontend.optimizer.GenIncOptimizer#AbsCollOps.*"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem]("org.scalajs.core.tools.linker.frontend.optimizer.GenIncOptimizer#AbsCollOps.*"),
      ProblemFilters.exclude[ReversedMissingMethodProblem]("org.scalajs.core.tools.linker.frontend.optimizer.GenIncOptimizer#AbsCollOps.*"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem]("org.scalajs.core.tools.linker.frontend.optimizer.IncOptimizer#CollOps.*"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem]("org.scalajs.core.tools.linker.frontend.optimizer.ParIncOptimizer#CollOps.*"),

      // private[emitter], not an issue
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.core.tools.linker.backend.emitter.JSGen.genIsInstanceOf"),

      // private, not an issue
      ProblemFilters.exclude[IncompatibleMethTypeProblem]("org.scalajs.core.tools.linker.checker.InfoChecker.this"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem]("org.scalajs.core.tools.linker.checker.IRChecker#CheckedClass.this")
  )

  val JSEnvs = Seq(
  )

  val JSEnvsTestKit = Seq(
  )

  val SbtPlugin = Seq(
  )

  val TestCommon = Seq(
  )

  val TestAdapter = TestCommon ++ Seq(
  )

  val CLI = Seq(
  )

  val Library = Seq(
      // New methods in a native JS trait, not an issue
      ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.scalajs.js.typedarray.TypedArrayStatic.*")
  )

  val TestInterface = Seq(
      /* Things that moved to scalajs-test-bridge.
       * They were private[scalajs] or stricter, so not an issue.
       */
      ProblemFilters.exclude[MissingClassProblem]("org.scalajs.testcommon.*"),
      ProblemFilters.exclude[MissingClassProblem]("org.scalajs.testinterface.HTMLRunner"),
      ProblemFilters.exclude[MissingClassProblem]("org.scalajs.testinterface.HTMLRunner$*"),
      ProblemFilters.exclude[MissingClassProblem]("org.scalajs.testinterface.TestDetector"),
      ProblemFilters.exclude[MissingClassProblem]("org.scalajs.testinterface.TestDetector$*"),
      ProblemFilters.exclude[MissingClassProblem]("org.scalajs.testinterface.internal.*")
  )
}
