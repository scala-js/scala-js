package build

import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
  )

  val Linker = Seq(
    // !!! Breaking, OK in minor release
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.linker.standard.LinkedClass.this"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.linker.standard.LinkedTopLevelExport.this"),
  )

  val LinkerInterface = Seq(
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
