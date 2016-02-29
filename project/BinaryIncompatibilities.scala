import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
      // Breaking! Trees.JSEnvInfo was removed.
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.ir.Trees$JSEnvInfo"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.ir.Trees$JSEnvInfo$")
  )

  val Tools = Seq(
      // private[emitter], not an issue
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.linker.backend.emitter.ScalaJSEnvHolder.strongmodeenv"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.linker.backend.emitter.ScalaJSClassEmitter.genStaticsES6Class")
  )

  val JSEnvs = Seq(
  )

  val SbtPlugin = Seq(
  )

  val TestAdapter = Seq(
  )

  val CLI = Seq(
  )

  val Library = Seq(
    // In theory, breaking, but this is an interface in runtime that no one should extend
    ProblemFilters.exclude[MissingMethodProblem](
        "scala.scalajs.runtime.EnvironmentInfo.javaSystemProperties"),
    ProblemFilters.exclude[MissingMethodProblem](
        "scala.scalajs.runtime.LinkingInfo.envInfo"),
    ProblemFilters.exclude[MissingMethodProblem](
        "scala.scalajs.runtime.LinkingInfo.scala$scalajs$runtime$LinkingInfo$_setter_$envInfo_="),
    ProblemFilters.exclude[MissingMethodProblem](
        "scala.scalajs.runtime.LinkingInfo.linkerVersion"),
    ProblemFilters.exclude[MissingMethodProblem](
        "scala.scalajs.runtime.LinkingInfo.scala$scalajs$runtime$LinkingInfo$_setter_$linkerVersion_=")
  )

  val TestInterface = Seq(
  )
}
