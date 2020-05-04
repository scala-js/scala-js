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
      // !! SemVer-Minor: breaking the standard linker API, requires a Minor version bump
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.standard.LinkedClass.this"),

      // !! Breaking the unstable linker API, allowed per our versioning policy
      exclude[MissingClassProblem](
          "org.scalajs.linker.analyzer.Infos$ClassInfo$"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.analyzer.Infos#ReachabilityInfo.apply"),
      exclude[IncompatibleMethTypeProblem](
          "org.scalajs.linker.backend.emitter.Emitter.this"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.Emitter.withOptimizeBracketSelects"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.Emitter.withTrackAllGlobalRefs"),

      // Additions to traits marked as not to be extended in user code, not an issue.
      exclude[ReversedMissingMethodProblem](
          "org.scalajs.linker.analyzer.Analysis#ClassInfo.jsNativeMembersUsed"),

      // private[linker], not an issue.
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.standard.LinkedClass.refined"),

      // private[closure], not an issue.
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.closure.ClosureAstTransformer.transformScript"),

      // private[emitter], not an issue.
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.ClassEmitter.org$scalajs$linker$backend$emitter$ClassEmitter$$codegenVarDef$default$4"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.ClassEmitter.org$scalajs$linker$backend$emitter$ClassEmitter$$codegenVarDef$default$5"),
      exclude[ReversedMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.GlobalKnowledge.getJSNativeLoadSpec"),
      exclude[MissingClassProblem](
          "org.scalajs.linker.backend.emitter.InternalOptions"),
      exclude[MissingClassProblem](
          "org.scalajs.linker.backend.emitter.InternalOptions$"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.avoidClashWithGlobalRef"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.codegenVar"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.codegenVar$default$3"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.codegenVarIdent"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.codegenVarIdent$default$3"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.encodeClassVar"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.esFeatures"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.envModuleField"),
      exclude[IncompatibleResultTypeProblem](
          "org.scalajs.linker.backend.emitter.JSGen.genSelectStatic"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.keepOnlyTrackedGlobalRefs"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.moduleKind"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.semantics"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.this"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.trackAllGlobalRefs"),
      exclude[IncompatibleMethTypeProblem](
          "org.scalajs.linker.backend.emitter.JSGen.this"),
      exclude[IncompatibleMethTypeProblem](
          "org.scalajs.linker.backend.emitter.KnowledgeGuardian.this"),

      // private, not an issue.
      exclude[IncompatibleResultTypeProblem](
          "org.scalajs.linker.analyzer.Analyzer.org$scalajs$linker$analyzer$Analyzer$$makeSyntheticMethodInfo$default$3"),
      exclude[IncompatibleResultTypeProblem](
          "org.scalajs.linker.analyzer.Analyzer.org$scalajs$linker$analyzer$Analyzer$$makeSyntheticMethodInfo$default$4"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.analyzer.Infos#ClassInfo.this"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.analyzer.Infos#ReachabilityInfo.this"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.closure.ClosureAstTransformer.this"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.checker.IRChecker#CheckedClass.this"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.Emitter#CoreJSLibCache.tree"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.CoreJSLib#CoreJSLibBuilder.org$scalajs$linker$backend$emitter$CoreJSLib$CoreJSLibBuilder$$defineStandardDispatcher$default$3$1"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.FunctionEmitter#JSDesugar.newSyntheticVar"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.FunctionEmitter#JSDesugar.resetSyntheticVarCounterIn"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.FunctionEmitter#JSDesugar.syntheticVarCounter"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.FunctionEmitter#JSDesugar.syntheticVarCounter_="),
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
