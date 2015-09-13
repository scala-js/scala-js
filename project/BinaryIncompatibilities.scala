import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
  )

  val Tools = Seq(
    // Breaking changes

    // Method does not do anything anymore
    ProblemFilters.exclude[MissingMethodProblem](
        "org.scalajs.core.tools.optimizer.Linker.clean"),

    // Previously deprecated ctor
    ProblemFilters.exclude[MissingMethodProblem](
        "org.scalajs.core.tools.optimizer.Linker.this"),

    // Binary Breaking (argument type widening)
    // Removes deprecated name
    ProblemFilters.exclude[IncompatibleMethTypeProblem](
        "org.scalajs.core.tools.optimizer.Linker.link"),

    // Private things, not an issue
    ProblemFilters.exclude[MissingClassProblem](
        "org.scalajs.core.tools.optimizer.Linker$PersistentIRFile"),
    ProblemFilters.exclude[MissingMethodProblem](
        "org.scalajs.core.tools.optimizer.Linker.org$scalajs$core$tools$optimizer$Linker$^dateFiles")
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
