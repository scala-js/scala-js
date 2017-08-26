import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
  )

  val Tools = Seq(
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
      /* RuntimeLong lost its `with java.io.Serializable`, but that is not a
       * problem because it also extends `java.lang.Number`, which itself
       * implements `java.io.Serializable` anyway.
       */
      ProblemFilters.exclude[MissingTypesProblem](
          "scala.scalajs.runtime.RuntimeLong")
  )

  val TestInterface = Seq(
  )
}
