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

    // !!! Breaking, PrimRef is not a case class anymore
    ProblemFilters.exclude[MissingTypesProblem]("org.scalajs.ir.Types$PrimRef"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Types#PrimRef.canEqual"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Types#PrimRef.productArity"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Types#PrimRef.productElement"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Types#PrimRef.productElementName"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Types#PrimRef.productElementNames"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Types#PrimRef.productIterator"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Types#PrimRef.productPrefix"),
    ProblemFilters.exclude[IncompatibleResultTypeProblem]("org.scalajs.ir.Types#PrimRef.unapply"),

    // !!! Breaking I guess ... we used to leak public things out of a `case class` with a private[ir] constructor
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Types#PrimRef.this"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Types#PrimRef.apply"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Types#PrimRef.copy"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Types#PrimRef.copy$default$1"),
    ProblemFilters.exclude[MissingTypesProblem]("org.scalajs.ir.Types$PrimRef$"),

    // constructor of a sealed abstract class, not an issue
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Types#PrimTypeWithRef.this"),

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
