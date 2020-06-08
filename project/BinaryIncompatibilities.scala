package build

import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
      // private, not an issue
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.ir.Serializers#Deserializer.readMemberDef"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.ir.Serializers#Deserializer.readMemberDefs"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.ir.Serializers#Deserializer.readTopLevelExportDef"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.ir.Serializers#Deserializer.readTopLevelExportDefs"),
  )

  val Linker = Seq(
      // Breaking in stable API. OK in Minor version.
      exclude[ProblemRef]("org.scalajs.linker.standard.*"),

      // Breaking in unstable packages
      exclude[ProblemRef]("org.scalajs.linker.analyzer.*"),
      exclude[ProblemRef]("org.scalajs.linker.backend.*"),
      exclude[ProblemRef]("org.scalajs.linker.frontend.*"),
  )

  val LinkerInterface = Seq(
  )

  val SbtPlugin = Seq(
      // Changes in LinkerImpl, which is declared that we can break it.
      exclude[ReversedMissingMethodProblem](
          "org.scalajs.sbtplugin.LinkerImpl.irFileCache"),
      exclude[FinalMethodProblem](
          "org.scalajs.sbtplugin.LinkerImpl#Reflect.irFileCache"),
      exclude[FinalMethodProblem](
          "org.scalajs.sbtplugin.LinkerImpl#Forwarding.irFileCache"),
  )

  val TestCommon = Seq(
  )

  val TestAdapter = TestCommon ++ Seq(
  )

  val Library = Seq(
      // Native types, not an issue.
      exclude[IncompatibleMethTypeProblem](
          "scala.scalajs.js.JSON.stringify"),
      exclude[IncompatibleResultTypeProblem](
          "scala.scalajs.js.JSON.stringify$default$3"),
      // New methods in sealed traits, not an issue.
      exclude[ReversedMissingMethodProblem](
          "scala.scalajs.js.LowPrioAnyImplicits.wrapMap"),
      exclude[ReversedMissingMethodProblem](
          "scala.scalajs.js.LowPrioAnyImplicits.wrapSet"),
  )

  val TestInterface = Seq(
  )
}
