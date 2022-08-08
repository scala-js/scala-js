package build

import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
    // private, not an issue
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Serializers#Hacks.*"),
  )

  val Linker = Seq(
    // Breaking! LinkedClass has one more argument in its constructor
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.linker.standard.LinkedClass.this"),
  )

  val LinkerInterface = Seq(
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
