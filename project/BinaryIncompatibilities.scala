import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val Tools = Seq(
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
