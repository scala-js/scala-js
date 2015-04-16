import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val Tools = Seq(
      // Protected, but in final class. Made private
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.classpath.PartialClasspath.resolveDependencies"),

      // Private, not an issue
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.sem.Semantics.this")
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
