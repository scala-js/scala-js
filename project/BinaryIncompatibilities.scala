package build

import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
    // Breaking, but in minor verison, so OK.
    exclude[Problem]("org.scalajs.ir.*"),
  )

  val Linker = Seq(
    // Breaking, but in minor version, so OK.
    exclude[Problem]("org.scalajs.linker.standard.*"),
  )

  val LinkerInterface = Seq(
    // Breaking, but in minor version, so OK.
    exclude[Problem]("org.scalajs.linker.interface.unstable.*"),

    // private, not an issue
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.linker.interface.Semantics.this"),
  )

  val SbtPlugin = Seq(
  )

  val TestAdapter = Seq(
  )

  val Library = Seq(
  )

  val TestInterface = Seq(
  )

  val JUnitRuntime = Seq(
  )
}
