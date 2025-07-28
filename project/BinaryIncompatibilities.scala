package build

import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
  )

  val Linker = Seq(
    // private[linker], not an issue
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.linker.standard.CoreSpec.linkTimeProperties"),
    ProblemFilters.exclude[MissingClassProblem]("org.scalajs.linker.standard.LinkTimeProperties*"),
  )

  val LinkerInterface = Seq(
  )

  val SbtPlugin = Seq(
  )

  val TestAdapter = Seq(
  )

  val Library = Seq(
    // private[reflect], not an issue
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.scalajs.reflect.LoadableModuleClass.this"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.scalajs.reflect.InstantiatableClass.this"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.scalajs.reflect.InvokableConstructor.this"),
  )

  val TestInterface = Seq(
  )

  val JUnitRuntime = Seq(
  )
}
