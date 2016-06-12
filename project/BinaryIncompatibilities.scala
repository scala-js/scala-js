import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
  )

  val Tools = Seq(
      // private, not an issue
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.OptimizerCore#PreTransBlock.this"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.OptimizerCore#PreTransBlock.apply"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.OptimizerCore#PreTransBlock.stats"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.OptimizerCore#PreTransBlock.result"),
      ProblemFilters.exclude[MissingTypesProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.OptimizerCore$PreTransTree"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.OptimizerCore#PreTransTree.apply"),
      ProblemFilters.exclude[MissingTypesProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.OptimizerCore$PreTransRecordTree"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.OptimizerCore$PreTransNoBlock"),
      ProblemFilters.exclude[MissingTypesProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.OptimizerCore$PreTransGenTree"),
      ProblemFilters.exclude[MissingTypesProblem](
          "org.scalajs.core.tools.linker.frontend.optimizer.OptimizerCore$PreTransLocalDef")
  )

  val JSEnvs = Seq(
      // Breaking changes
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.jsenv.phantomjs.PhantomJSEnv#AbstractPhantomRunner.codeCache"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.jsenv.phantomjs.PhantomJSEnv#AbstractPhantomRunner.org$" +
          "scalajs$jsenv$phantomjs$PhantomJSEnv$AbstractPhantomRunner$_setter_$codeCache_=")
  )

  val JSEnvsTestKit = Seq(
      // Breaking changes
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.jsenv.test.NodeJSWithCustomInitFilesTest"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.jsenv.test.PhantomJSWithCustomInitFilesTest"),

      // Source compatible, not a problem.
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.jsenv.test.TimeoutTests.timeoutSingleArgTest"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.jsenv.test.BasicJSEnvTests.allowScriptTags")
  )

  val SbtPlugin = Seq(
      // private, not an issue
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.sbtplugin.Implicits$SbtLoggerWrapper")
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
