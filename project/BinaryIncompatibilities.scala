package build

import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
    // ! Breaking, ok in minor version
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Trees#ClassDef.this"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Trees#ClassDef.apply"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Trees#ClassDef.jsNativeMembers"),
    ProblemFilters.exclude[MissingTypesProblem]("org.scalajs.ir.Trees$JSNativeMemberDef"),

    // private, not an issue
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Serializers#Serializer.writeMemberDefs"),
  )

  val Linker = Seq(
  )

  val LinkerInterface = Seq(
  )

  val SbtPlugin = Seq(
  )

  val TestAdapter = Seq(
    // private[testing], not an issue
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.testing.common.RPCCore.this"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.testing.common.RPCCore.handleMessage"),
  )

  val Library = Seq(
  )

  val TestInterface = Seq(
  )

  val JUnitRuntime = Seq(
  )
}
