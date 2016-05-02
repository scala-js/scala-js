import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
  )

  val Tools = Seq(
  )

  val JSEnvs = Seq(
  )

  val JSEnvsTestKit = Seq(
      // Breaking changes
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.jsenv.test.NodeJSWithCustomInitFilesTest"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.jsenv.test.PhantomJSWithCustomInitFilesTest")
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
