package build

import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
  )

  val Logging = Seq(
  )

  val Linker = Seq(
      // private[emitter], not an issue.
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.ClassEmitter.org$scalajs$linker$backend$emitter$ClassEmitter$$classVarDef$default$5"),
      exclude[ReversedMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.GlobalKnowledge.methodsInObject"),

      // private[linker], not an issue.
      exclude[MissingClassProblem]("org.scalajs.linker.NodeFS$FS$"),

      // private, not an issue.
      exclude[MissingClassProblem](
          "org.scalajs.linker.NodeIRContainer$Path$"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.KnowledgeGuardian#SpecialInfo.this"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.backend.emitter.KnowledgeGuardian#SpecialInfo.update"),
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
