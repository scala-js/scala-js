package build

import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
      // Breaking in ir API. OK in Minor version.
      exclude[ProblemRef]("org.scalajs.ir.*"),
  )

  val Linker = Seq(
      // Breaking in stable API. OK in Minor version.
      exclude[ProblemRef]("org.scalajs.linker.standard.*"),

      // Breaking in unstable packages
      exclude[ProblemRef]("org.scalajs.linker.analyzer.*"),
      exclude[ProblemRef]("org.scalajs.linker.backend.*"),
      exclude[ProblemRef]("org.scalajs.linker.checker.*"),
      exclude[ProblemRef]("org.scalajs.linker.frontend.*"),

      /* Protected inheritance through private[interface] ctor, not an issue.
       * Unclear why this surfaces in Linker, but it does.
       */
      exclude[FinalMethodProblem](
          "org.scalajs.linker.interface.Linker.link"),

      // private, not an issue.
      exclude[MissingClassProblem](
          "org.scalajs.linker.MemOutputFile$MemFileImpl"),
      exclude[MissingClassProblem](
          "org.scalajs.linker.NodeOutputFile$NodeOutputFileImpl"),
      exclude[MissingClassProblem](
          "org.scalajs.linker.PathOutputFile$AtomicPathOutputFileImpl"),
      exclude[MissingClassProblem](
          "org.scalajs.linker.PathOutputFile$PathOutputFileImpl"),
  )

  val LinkerInterface = Seq(
      // Breaking in unstable API. OK in Minor version.
      exclude[ProblemRef]("org.scalajs.linker.interface.unstable.*"),

      // Protected inheritance through private[interface] ctor, not an issue.
      exclude[FinalMethodProblem](
          "org.scalajs.linker.interface.Linker.link"),
      exclude[ReversedMissingMethodProblem](
          "org.scalajs.linker.interface.Linker.link"),
      exclude[FinalClassProblem](
          "org.scalajs.linker.interface.ModuleInitializer"),

      // private[interface], not an issue.
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.interface.ModuleInitializer.impl"),
      exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.interface.ModuleInitializer.this"),
  )

  val SbtPlugin = Seq(
      // Changes in LinkerImpl, which is declared that we can break it.
      exclude[ReversedMissingMethodProblem](
          "org.scalajs.sbtplugin.LinkerImpl.outputDirectory"),
      exclude[FinalMethodProblem](
          "org.scalajs.sbtplugin.LinkerImpl#Reflect.outputFile"),
      exclude[FinalMethodProblem](
          "org.scalajs.sbtplugin.LinkerImpl#Forwarding.outputFile"),
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
