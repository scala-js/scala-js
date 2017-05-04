import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
  )

  val Tools = Seq(
      /* Breaking: the 3-way decomposition is not available anymore.
       * Only emitAll is kept as public API.
       */
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.tools.linker.backend.emitter.Emitter.emit"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.tools.linker.backend.emitter.Emitter.emitPostlude"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.tools.linker.backend.emitter.Emitter.emitPrelude"),

      // private[emitter], not an issue
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.tools.linker.backend.emitter.FunctionEmitter.desugarToFunction"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.tools.linker.backend.emitter.FunctionEmitter.desugarToFunction"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.tools.linker.backend.emitter.FunctionEmitter.desugarTree"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.tools.linker.backend.emitter.FunctionEmitter#JSDesugar.desugarToFunction"),

      // private[emitter], not an issue
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "org.scalajs.core.tools.linker.backend.emitter.ClassEmitter.buildClass"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.tools.linker.backend.emitter.ClassEmitter.genAddToObject"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.tools.linker.backend.emitter.ClassEmitter.genAddToPrototype"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.tools.linker.backend.emitter.ClassEmitter.genClassExports"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.tools.linker.backend.emitter.ClassEmitter.genConstructor"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.tools.linker.backend.emitter.ClassEmitter.genConstructorExportDef"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.tools.linker.backend.emitter.ClassEmitter.genCreateStaticFieldsOfScalaClass"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.tools.linker.backend.emitter.ClassEmitter.genDefaultMethod"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.tools.linker.backend.emitter.ClassEmitter.genDefaultMethods"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.tools.linker.backend.emitter.ClassEmitter.genExportedMembers"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.tools.linker.backend.emitter.ClassEmitter.genMethod"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.tools.linker.backend.emitter.ClassEmitter.genProperty"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.tools.linker.backend.emitter.ClassEmitter.genPropertyName"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.tools.linker.backend.emitter.ClassEmitter.genStaticInitialization"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.tools.linker.backend.emitter.ClassEmitter.genStaticMembers"),

      // private[emitter], not an issue
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.tools.linker.backend.emitter.JSGen.this"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.tools.linker.backend.emitter.JSGen.transformIdent"),

      // private[emitter], not an issue
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.tools.linker.backend.emitter.Emitter#MethodCache.getOrElseUpdate")
  )

  val JSEnvs = Seq(
  )

  val JSEnvsTestKit = Seq(
  )

  val SbtPlugin = Seq(
  )

  val TestAdapter = Seq(
  )

  val CLI = Seq(
  )

  val Library = Seq(
  )

  val TestInterface = Seq(
  )
}
