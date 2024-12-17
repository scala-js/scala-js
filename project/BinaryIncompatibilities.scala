package build

import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
    // !!! Breaking, OK in minor release
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Printers#IRTreePrinter.print"),
    ProblemFilters.exclude[MissingClassProblem]("org.scalajs.ir.Trees$ArrayLength"),
    ProblemFilters.exclude[MissingClassProblem]("org.scalajs.ir.Trees$ArrayLength$"),
    ProblemFilters.exclude[MissingClassProblem]("org.scalajs.ir.Trees$Clone"),
    ProblemFilters.exclude[MissingClassProblem]("org.scalajs.ir.Trees$Clone$"),
    ProblemFilters.exclude[MissingClassProblem]("org.scalajs.ir.Trees$GetClass"),
    ProblemFilters.exclude[MissingClassProblem]("org.scalajs.ir.Trees$GetClass$"),
    ProblemFilters.exclude[MissingClassProblem]("org.scalajs.ir.Trees$IdentityHashCode"),
    ProblemFilters.exclude[MissingClassProblem]("org.scalajs.ir.Trees$IdentityHashCode$"),
    ProblemFilters.exclude[MissingClassProblem]("org.scalajs.ir.Trees$JSLinkingInfo"),
    ProblemFilters.exclude[MissingClassProblem]("org.scalajs.ir.Trees$JSLinkingInfo$"),
    ProblemFilters.exclude[MissingClassProblem]("org.scalajs.ir.Trees$LabelIdent"),
    ProblemFilters.exclude[MissingClassProblem]("org.scalajs.ir.Trees$LabelIdent$"),
    ProblemFilters.exclude[Problem]("org.scalajs.ir.Trees#Labeled.*"),
    ProblemFilters.exclude[Problem]("org.scalajs.ir.Trees#Return.*"),
    ProblemFilters.exclude[MissingClassProblem]("org.scalajs.ir.Trees$This"),
    ProblemFilters.exclude[MissingTypesProblem]("org.scalajs.ir.Trees$This$"),
    ProblemFilters.exclude[IncompatibleResultTypeProblem]("org.scalajs.ir.Trees#This.apply"),
    ProblemFilters.exclude[IncompatibleMethTypeProblem]("org.scalajs.ir.Trees#This.unapply"),
    ProblemFilters.exclude[MissingClassProblem]("org.scalajs.ir.Trees$Throw"),
    ProblemFilters.exclude[MissingClassProblem]("org.scalajs.ir.Trees$Throw$"),
    ProblemFilters.exclude[MissingClassProblem]("org.scalajs.ir.Trees$UnwrapFromThrowable"),
    ProblemFilters.exclude[MissingClassProblem]("org.scalajs.ir.Trees$UnwrapFromThrowable$"),
    ProblemFilters.exclude[Problem]("org.scalajs.ir.Trees#VarRef.*"),
    ProblemFilters.exclude[MissingClassProblem]("org.scalajs.ir.Trees$WrapAsThrowable"),
    ProblemFilters.exclude[MissingClassProblem]("org.scalajs.ir.Trees$WrapAsThrowable$"),
    ProblemFilters.exclude[IncompatibleResultTypeProblem]("org.scalajs.ir.Trees#*.tpe"),
    ProblemFilters.exclude[MissingClassProblem]("org.scalajs.ir.Types$NoType$"),

    // !!! Breaking, OK in minor release
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Transformers#Transformer.transform"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Transformers#Transformer.transformExpr"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Transformers#Transformer.transformExprOrJSSpread"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Transformers#Transformer.transformStat"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Transformers#Transformer.transformStats"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Trees#Transient#Value.transform"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("org.scalajs.ir.Trees#Transient#Value.transform"),

    // private, not an issue
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Serializers#Deserializer.readLabelIdent"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Serializers#Serializer.writeLabelIdent"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Hashers#TreeHasher.mixLabelIdent"),
  )

  val Linker = Seq(
  )

  val LinkerInterface = Seq(
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
