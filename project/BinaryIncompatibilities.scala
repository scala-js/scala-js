import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
  )

  val Tools = Seq(
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
