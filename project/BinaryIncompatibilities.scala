import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
      // Breaking: FieldDef has new field `static`
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.ir.Trees#FieldDef.this"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.ir.Trees#FieldDef.apply"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.ir.Trees#FieldDef.copy"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.ir.Trees#FieldDef.copy$default$1"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.ir.Trees#FieldDef.copy$default$2"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.ir.Trees#FieldDef.copy$default$3"),

      // Breaking: PropertyDef has new field `static`
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.ir.Trees#PropertyDef.this"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.ir.Trees#PropertyDef.apply"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.ir.Trees#PropertyDef.copy"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.ir.Trees#PropertyDef.copy$default$1"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.ir.Trees#PropertyDef.copy$default$2"),

      // Breaking: TopLevelExportDef has been renamed to TopLevelMethodExportDef
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.ir.Trees$TopLevelExportDef"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.ir.Trees$TopLevelExportDef$"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.ir.Tags.TagTopLevelExportDef"),

      // Breaking: PropertyName.{name -> encodedName}
      ProblemFilters.exclude[ReversedMissingMethodProblem](
          "org.scalajs.core.ir.Trees#PropertyName.encodedName"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.ir.Trees#StringLiteral.name"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.ir.Trees#PropertyName.name"),

      // private, not an issue
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.ir.Infos#MethodInfo.this"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.ir.Infos#GenInfoTraverser.generateClassExportsInfo")
  )

  val Tools = Seq(
      // Breaking: add 2 methods to the GenLinker trait
      ProblemFilters.exclude[ReversedMissingMethodProblem](
          "org.scalajs.core.tools.linker.GenLinker.link"),
      ProblemFilters.exclude[ReversedMissingMethodProblem](
          "org.scalajs.core.tools.linker.GenLinker.linkUnit"),

      // Breaking. Remove PropertyName.name
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.tools.javascript.Trees#PropertyName.name"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.tools.javascript.Trees#StringLiteral.name"),

      // private, not an issue
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.tools.sem.Semantics.this"),

      // private, not an issue
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.tools.linker.checker.IRChecker#CheckedField.this"),

      // private, not an issue
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.tools.linker.analyzer.Analyzer.org$scalajs$core$tools$linker$analyzer$Analyzer$$createMissingMethodInfo$default$2"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.tools.linker.analyzer.Analyzer.org$scalajs$core$tools$linker$analyzer$Analyzer$$createMissingMethodInfo$default$3"),

      // private[frontend], not an issue
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.tools.linker.frontend.BaseLinker.linkInternal"),

      // private[closure], not an issue
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "org.scalajs.core.tools.linker.backend.closure.ClosureAstTransformer.transformString"),

      // private[emitter], not an issue
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.emitter.ScalaJSClassEmitter$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.emitter.ScalaJSClassEmitter"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.emitter.JSDesugaring"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.emitter.JSDesugaring$DesugarException"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.emitter.JSDesugaring$MyTreeOps"),

      // private, not an issue
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.emitter.JSDesugaring$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.emitter.JSDesugaring$JSDesugar"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.emitter.JSDesugaring$JSDesugar$RecordFieldVarRef$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.emitter.JSDesugaring$JSDesugar$RecordVarRef$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.emitter.JSDesugaring$JSDesugar$RecordAwareEnv"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.emitter.JSDesugaring$Lhs"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.emitter.JSDesugaring$Lhs$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.emitter.JSDesugaring$Lhs$Assign"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.emitter.JSDesugaring$Lhs$Assign$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.emitter.JSDesugaring$Lhs$Return"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.emitter.JSDesugaring$Lhs$Return$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.emitter.JSDesugaring$Lhs$Discard$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.emitter.JSDesugaring$Lhs$VarDef$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.emitter.JSDesugaring$Lhs$VarDef"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.emitter.JSDesugaring$Env"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.emitter.JSDesugaring$Env$")
  )

  val JSEnvs = Seq(
  )

  val JSEnvsTestKit = Seq(
  )

  val SbtPlugin = Seq(
    // private[scalajs], not an issue.
    ProblemFilters.exclude[DirectMissingMethodProblem](
        "org.scalajs.sbtplugin.HTMLRunnerTemplate.render")
  )

  val TestAdapter = Seq(
  )

  val CLI = Seq(
      // private, not an issue
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.cli.Scalajsld#Options.this"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsld#Options.<init>$default$2"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsld#Options.<init>$default$3"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsld#Options.<init>$default$4"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsld#Options.<init>$default$5"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsld#Options.<init>$default$6"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsld#Options.<init>$default$7"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsld#Options.<init>$default$11"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsld#Options.<init>$default$12"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsld#Options.<init>$default$14"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsld#Options.<init>$default$15"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.cli.Scalajsld#Options.copy"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsld#Options.copy$default$2"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsld#Options.copy$default$3"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsld#Options.copy$default$4"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsld#Options.copy$default$5"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsld#Options.copy$default$6"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsld#Options.copy$default$7"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsld#Options.copy$default$11"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsld#Options.copy$default$12"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsld#Options.copy$default$14"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsld#Options.copy$default$15"),
      ProblemFilters.exclude[MissingTypesProblem](
          "org.scalajs.cli.Scalajsld$Options$"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.cli.Scalajsld#Options.apply"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsld#Options.apply$default$2"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsld#Options.apply$default$3"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsld#Options.apply$default$4"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsld#Options.apply$default$5"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsld#Options.apply$default$6"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsld#Options.apply$default$7"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsld#Options.apply$default$11"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsld#Options.apply$default$12"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsld#Options.apply$default$14"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsld#Options.apply$default$15")
  )

  val Library = Seq(
      // Relaxed typing (js.Iterable instead of js.Array) in js.Promise.
      // Not a compatibility issue (due to JS land).
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "scala.scalajs.js.Promise.race"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "scala.scalajs.js.Promise.all"),

      // js.Iterable support in TypedArray.
      // Not a compatibility issue (due to JS land).
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "scala.scalajs.js.typedarray.Int8Array.set"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "scala.scalajs.js.typedarray.Int8Array.this"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "scala.scalajs.js.typedarray.Uint8ClampedArray.set"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "scala.scalajs.js.typedarray.Uint8ClampedArray.this"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "scala.scalajs.js.typedarray.Uint16Array.set"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "scala.scalajs.js.typedarray.Uint16Array.this"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "scala.scalajs.js.typedarray.Uint8Array.set"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "scala.scalajs.js.typedarray.Uint8Array.this"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "scala.scalajs.js.typedarray.TypedArray.set"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "scala.scalajs.js.typedarray.TypedArray.set"),
      ProblemFilters.exclude[ReversedMissingMethodProblem](
          "scala.scalajs.js.typedarray.TypedArray.set"),
      ProblemFilters.exclude[ReversedMissingMethodProblem](
          "scala.scalajs.js.typedarray.TypedArray.jsIterator"),
      ProblemFilters.exclude[InheritedNewAbstractMethodProblem](
          "scala.scalajs.js.Iterable.jsIterator"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "scala.scalajs.js.typedarray.Int32Array.set"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "scala.scalajs.js.typedarray.Int32Array.this"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "scala.scalajs.js.typedarray.Float32Array.set"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "scala.scalajs.js.typedarray.Float32Array.this"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "scala.scalajs.js.typedarray.Float64Array.set"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "scala.scalajs.js.typedarray.Float64Array.this"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "scala.scalajs.js.typedarray.Int16Array.set"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "scala.scalajs.js.typedarray.Int16Array.this"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "scala.scalajs.js.typedarray.Uint32Array.set"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "scala.scalajs.js.typedarray.Uint32Array.this"),

      // private[js], not an issue
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "scala.scalajs.js.Dictionary.rawApply"),

      // New member in non-sealed trait (for low prio implicits).
      // Theoretically breaking.
      ProblemFilters.exclude[InheritedNewAbstractMethodProblem](
          "scala.scalajs.js.LowestPrioAnyImplicits.iterableOps"),

      // New members of an @js.native trait in `runtime`, not an issue
      ProblemFilters.exclude[ReversedMissingMethodProblem](
          "scala.scalajs.runtime.LinkingInfo#Semantics.arrayIndexOutOfBounds"),
      ProblemFilters.exclude[ReversedMissingMethodProblem](
          "scala.scalajs.runtime.LinkingInfo#Semantics.scala$scalajs$runtime$LinkingInfo$Semantics$_setter_$arrayIndexOutOfBounds_=")
  )

  val TestInterface = Seq(
  )
}
