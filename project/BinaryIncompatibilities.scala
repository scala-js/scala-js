package build

import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
    // !!! Breaking, OK in minor release
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Names.*Class"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Names.ClassInitializerName"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Names.DefaultModuleID"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Names.HijackedClasses"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Names.NoArgConstructorName"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Names.ObjectArgConstructorName"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Names.StaticInitializerName"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Types.BoxedClassToPrimType"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Types.PrimTypeToBoxedClass"),

    // private, not an issue
    ProblemFilters.exclude[MissingClassProblem]("org.scalajs.ir.Serializers$Deserializer$BodyHack5Transformer$"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Serializers#Hacks.use*"),
  )

  val Linker = Seq(
    // !!! Breaking, OK in minor release
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.linker.standard.LinkedClass.this"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.linker.standard.LinkedTopLevelExport.this"),
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
