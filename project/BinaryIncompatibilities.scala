package build

import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
  )

  val Linker = Seq(
      // Breaking in the unstable API.
      exclude[IncompatibleResultTypeProblem](
          "org.scalajs.linker.backend.emitter.Emitter#Result.body"),
      exclude[IncompatibleMethTypeProblem](
          "org.scalajs.linker.backend.javascript.Trees#Block.apply"),

      // private[closure], not an issue.
      exclude[IncompatibleMethTypeProblem](
          "org.scalajs.linker.backend.closure.ClosureAstTransformer.transformScript"),

      // private[emitter], not an issue.
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.ClassEmitter.org$scalajs$linker$backend$emitter$ClassEmitter$$classVarDef$default$4"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.ClassEmitter.org$scalajs$linker$backend$emitter$ClassEmitter$$classVarDef$default$5"),
      exclude[IncompatibleMethTypeProblem](
          "org.scalajs.linker.backend.emitter.ClassEmitter.this"),
      exclude[IncompatibleMethTypeProblem](
          "org.scalajs.linker.backend.emitter.CoreJSLib.build"),
      exclude[IncompatibleMethTypeProblem](
          "org.scalajs.linker.backend.emitter.FunctionEmitter.this"),
      exclude[ReversedMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.GlobalKnowledge.methodsInObject"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.envModuleFieldIdent"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.genArrayValue"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.genAsInstanceOf"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.genBoxedCharZero"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.genBoxedZeroOf"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.genCallHelper"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.genClassDataOf"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.genClassDataOf"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.genClassOf"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.genClassOf"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.genIsInstanceOf"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.genIsInstanceOfHijackedClass"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.genJSClassConstructor"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.genJSClassConstructor"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.genJSPrivateFieldIdent"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.genJSPrivateSelect"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.genLoadJSFromSpec"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.genLoadModule"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.genLongModuleApply"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.genLongZero"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.genNonNativeJSClassConstructor"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.genPropSelect"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.genScalaClassNew"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.genSelect"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.genSelect"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.genSelectStatic"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.genZeroOf"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.nameGen"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.this"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.useBigIntForLongs"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.varGen"),
      exclude[IncompatibleMethTypeProblem](
          "org.scalajs.linker.backend.emitter.Emitter#Result.this"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.VarGen.classVarIdent"),
      exclude[IncompatibleMethTypeProblem](
          "org.scalajs.linker.backend.emitter.VarGen.classVarIdent"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.VarGen.coreJSLibVarIdent"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.VarGen.coreJSLibVarIdent"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.VarGen.this"),

      // private[linker], not an issue.
      exclude[MissingClassProblem]("org.scalajs.linker.NodeFS$FS$"),

      // private, not an issue.
      exclude[MissingClassProblem](
          "org.scalajs.linker.NodeIRContainer$Path$"),
      exclude[IncompatibleMethTypeProblem](
          "org.scalajs.linker.backend.emitter.CoreJSLib#CoreJSLibBuilder.this"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.Emitter#State.jsGen"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.KnowledgeGuardian#SpecialInfo.this"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.KnowledgeGuardian#SpecialInfo.update"),
  )

  val LinkerInterface = Seq(
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
