package build

import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
  )

  val Linker = Seq(
    // private[linker], not an issue
    exclude[MissingClassProblem](
        "org.scalajs.linker.standard.SymbolRequirement$Nodes$Optional"),
    exclude[MissingClassProblem](
        "org.scalajs.linker.standard.SymbolRequirement$Nodes$Optional$"),
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
