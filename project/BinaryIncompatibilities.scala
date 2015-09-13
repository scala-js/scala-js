import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
  )

  val Tools = Seq(
    // Breaking changes
    ProblemFilters.exclude[MissingClassProblem](
        "org.scalajs.core.tools.optimizer.Linker"),

    // Private things, not an issue
    ProblemFilters.exclude[MissingClassProblem](
        "org.scalajs.core.tools.optimizer.Linker$PersistentIRFile")
  )

  val JSEnvs = Seq(
  )

  val SbtPlugin = Seq(
  )

  val TestAdapter = Seq(
  )

  val CLI = Seq(
  )
}
