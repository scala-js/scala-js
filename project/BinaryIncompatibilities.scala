package build

import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
    // Breaking
    ProblemFilters.exclude[IncompatibleMethTypeProblem]("org.scalajs.core.ir.Infos#ClassInfoBuilder.addInterfaces")
  )

  val Tools = Seq(
      // private[linker], not an issue
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.tools.linker.StandardLinker#Config.outputMode"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.tools.linker.StandardLinker#Config.withOutputMode"),
//      // Were using Traversable, which has been removed in 2.13
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "org.scalajs.core.tools.linker.checker.IRChecker#CheckedClass.this"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "org.scalajs.core.tools.linker.analyzer.SymbolRequirement#Factory.callMethods"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "org.scalajs.core.tools.linker.analyzer.SymbolRequirement#Factory.instantiateClass"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "org.scalajs.core.tools.linker.analyzer.SymbolRequirement#Factory.callOnModule"),
      // Were using GenIterable, which has been removed in 2.13
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.IncOptimizer#CollOps.parFlatMapKeys"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.IncOptimizer#CollOps.getAcc"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.GenIncOptimizer#Class.subclasses_="),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.GenIncOptimizer#Class.subclasses"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.GenIncOptimizer#AbsCollOps.parFlatMapKeys"),
      ProblemFilters.exclude[ReversedMissingMethodProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.GenIncOptimizer#AbsCollOps.parFlatMapKeys"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.GenIncOptimizer#AbsCollOps.prepAdd"),
      ProblemFilters.exclude[ReversedMissingMethodProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.GenIncOptimizer#AbsCollOps.prepAdd"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.GenIncOptimizer#AbsCollOps.finishAdd"),
      ProblemFilters.exclude[ReversedMissingMethodProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.GenIncOptimizer#AbsCollOps.finishAdd"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.GenIncOptimizer#AbsCollOps.getAcc"),
      ProblemFilters.exclude[ReversedMissingMethodProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.GenIncOptimizer#AbsCollOps.getAcc"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.GenIncOptimizer#AbsCollOps.emptyParIterable"),
      ProblemFilters.exclude[ReversedMissingMethodProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.GenIncOptimizer#AbsCollOps.emptyParIterable"),
      // Were using GenMap, which has been removed in 2.13
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.GenIncOptimizer#AbsCollOps.put"),
      ProblemFilters.exclude[ReversedMissingMethodProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.GenIncOptimizer#AbsCollOps.put"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.GenIncOptimizer#AbsCollOps.retain"),
      ProblemFilters.exclude[ReversedMissingMethodProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.GenIncOptimizer#AbsCollOps.retain"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.GenIncOptimizer#AbsCollOps.remove"),
      ProblemFilters.exclude[ReversedMissingMethodProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.GenIncOptimizer#AbsCollOps.remove"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.GenIncOptimizer#AbsCollOps.emptyParMap"),
      ProblemFilters.exclude[ReversedMissingMethodProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.GenIncOptimizer#AbsCollOps.emptyParMap"),
      // Temporary, should be removed after scala-parallel-collection supports 2.13
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.ParIncOptimizer$ParMethodImpl"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.ParIncOptimizer"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.ParIncOptimizer$ParInterfaceType"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.ParIncOptimizer$CollOps$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.ParIncOptimizer$"),
      // Added methods
      ProblemFilters.exclude[ReversedMissingMethodProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.GenIncOptimizer#AbsCollOps.filter")
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
  )

  val TestInterface = TestCommon ++ Seq(
  )
}
