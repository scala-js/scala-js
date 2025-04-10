package build

import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
  )

  val Linker = Seq(
  )

  val LinkerInterface = Seq(
  )

  val SbtPlugin = Seq(
  )

  val TestAdapter = Seq(
  )

  val Library = Seq(
    // private[reflect], not an issue
    ProblemFilters.exclude[IncompatibleMethTypeProblem]("scala.scalajs.reflect.InvokableConstructor.this"),
    ProblemFilters.exclude[IncompatibleMethTypeProblem]("scala.scalajs.reflect.LoadableModuleClass.this"),
  )

  val TestInterface = Seq(
  )

  val JUnitRuntime = Seq(
  )
}
