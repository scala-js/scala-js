package build

import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
      // private, not an issue.
      exclude[DirectMissingMethodProblem](
          "org.scalajs.ir.Serializers#Deserializer.readMemberDef"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.ir.Serializers#Deserializer.readMemberDefs"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.ir.Serializers#Deserializer.readTopLevelExportDef"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.ir.Serializers#Deserializer.readTopLevelExportDefs"),
  )

  val Logging = Seq(
  )

  val Linker = Seq(
      // private[closure], not an issue.
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.closure.ClosureAstTransformer.transformScript"),

      // private, not an issue.
      exclude[DirectMissingMethodProblem](
        "org.scalajs.linker.backend.closure.ClosureAstTransformer.this"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.Emitter#CoreJSLibCache.tree"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.CoreJSLib#CoreJSLibBuilder.org$scalajs$linker$backend$emitter$CoreJSLib$CoreJSLibBuilder$$defineStandardDispatcher$default$3$1"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.FunctionEmitter#JSDesugar.resetSyntheticVarCounterIn"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.frontend.optimizer.OptimizerCore#Intrinsics.IdentityHashCode"),
  )

  val LinkerInterface = Seq(
  )

  val JSEnvs = Seq(
  )

  val JSEnvsTestKit = Seq(
  )

  val SbtPlugin = Seq(
  )

  val TestCommon = Seq(
  )

  val TestAdapter = TestCommon ++ Seq(
  )

  val Library = Seq(
  )

  val TestInterface = Seq(
  )
}
