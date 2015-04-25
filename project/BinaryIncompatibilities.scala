import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
      // Breaking: ParamDef has an additinal `rest` parameter
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.ir.Trees#ParamDef.apply"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.ir.Trees#ParamDef.copy"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.ir.Trees#ParamDef.this")
  )

  val Tools = Seq(
      // Breaking: js.ParamDef has an additinal `rest` parameter
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.javascript.Trees#ParamDef.apply"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.javascript.Trees#ParamDef.copy"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.javascript.Trees#ParamDef.this"),

      // Breaking: Optimizer factories take an additional OutputMode parameter
      // We favored source compatibility over binary compatibility
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "org.scalajs.core.tools.optimizer.ScalaJSOptimizer.this"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.tools.optimizer.ParIncOptimizer.factory"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.tools.optimizer.IncOptimizer.factory"),

      // Protected, but in final class. Made private
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.classpath.PartialClasspath.resolveDependencies"),

      // Private, not an issue
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.sem.Semantics.this"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.optimizer.IRChecker#Env.withArgumentsVar"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.optimizer.OptimizerCore.this"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.javascript.JSDesugaring#JSDesugar.this"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.javascript.JSDesugaring#JSDesugar.doVarDef"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.tools.jsdep.ConflictingNameException.org$scalajs$core$tools$jsdep$ConflictingNameException$$mkMsg")
  )

  val TestAdapter = Seq(
      // Private things, not an issue
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.testadapter.ScalaJSRunner.org$scalajs$testadapter$ScalaJSRunner$$awaitSlaveRunner"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.testadapter.ScalaJSRunner.org$scalajs$testadapter$ScalaJSRunner$$launchNewSlave"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.testadapter.ScalaJSRunner.org$scalajs$testadapter$ScalaJSRunner$$createSlaveRunner"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.testadapter.ScalaJSRunner.org$scalajs$testadapter$ScalaJSRunner$$createSlave")
  )
}
