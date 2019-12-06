package build

import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
  )

  val Logging = Seq(
  )

  val Linker = Seq(
      // breaking in unstable Linker API.
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.Emitter.emitAll"),
      exclude[MissingClassProblem](
          "org.scalajs.linker.backend.javascript.JSBuilder"),
      exclude[MissingClassProblem](
          "org.scalajs.linker.backend.javascript.JSFileBuilder"),
      exclude[MissingClassProblem](
          "org.scalajs.linker.backend.javascript.JSFileBuilderWithSourceMap"),
      exclude[MissingClassProblem](
          "org.scalajs.linker.backend.javascript.JSLineBuilder"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.javascript.Printers#JSTreePrinter.complete"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.javascript.Printers#JSTreePrinterWithSourceMap.complete"),

      // private[backend], not an issue
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.Emitter.emitForClosure"),

      // private[closure], not an issue.
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.closure.ClosureAstTransformer.setNodePosition"),
      exclude[MissingClassProblem](
          "org.scalajs.linker.backend.closure.ClosureModuleBuilder"),
      exclude[MissingClassProblem](
          "org.scalajs.linker.backend.closure.ClosureModuleBuilder$"),

      // private[emitter], not an issue.
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.ClassEmitter.genModuleAccessor"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.CoreJSLib.build"),
      exclude[ReversedMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.GlobalKnowledge.hijackedClassHasPublicMethod"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.genName"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.genOriginalName"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.keepOnlyTrackedGlobalRefs"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.JSGen.this"),
      exclude[MissingClassProblem](
          "org.scalajs.linker.backend.emitter.JSGen$"),

      // private[javascript], not an issue.
      exclude[FinalClassProblem](
          "org.scalajs.linker.backend.javascript.SourceMapWriter"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.javascript.SourceMapWriter.<init>$default$3"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.javascript.SourceMapWriter.complete"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.javascript.SourceMapWriter.jsFileURI"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.javascript.SourceMapWriter.out"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.javascript.SourceMapWriter.relativizeBaseURI"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.javascript.SourceMapWriter.this"),

      // private, not an issue.
      exclude[MissingClassProblem](
          "org.scalajs.linker.backend.closure.ClosureModuleBuilder$ScalaJSSourceAst"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.CoreJSLib#CoreJSLibBuilder.this"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.Emitter#DesugaredClassCache.moduleAccessor"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.Emitter#State.coreJSLib"),
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
