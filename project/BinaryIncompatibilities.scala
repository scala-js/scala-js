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
      // private[scalajs], not an issue.
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testadapter.TaskDefSerializers"),

      // private[testadapter], not an issue.
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testadapter.ComUtils"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testadapter.ComUtils$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testadapter.EventSerializers"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testadapter.EventSerializers$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testadapter.EventSerializers$DeserializedEvent"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testadapter.EventSerializers$EventDeserializer$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testadapter.FingerprintSerializers"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testadapter.FingerprintSerializers$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testadapter.FingerprintSerializers$DeserializedAnnotatedFingerprint"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testadapter.FingerprintSerializers$DeserializedSubclassFingerprint"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testadapter.FingerprintSerializers$FingerprintDeserializer$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testadapter.FingerprintSerializers$FingerprintSerializer$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testadapter.FrameworkInfo"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testadapter.FrameworkInfo$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testadapter.FrameworkInfo$Deserializer$"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.testadapter.ScalaJSRunner.msgHandler"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.testadapter.ScalaJSRunner.getSlave"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.testadapter.ScalaJSTask.fromInfo"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.testadapter.ScalaJSTask.this"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testadapter.SelectorSerializers"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testadapter.SelectorSerializers$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testadapter.SelectorSerializers$SelectorDeserializer$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testadapter.SelectorSerializers$SelectorSerializer$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testadapter.TaskDefSerializers$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testadapter.TaskDefSerializers$TaskDefDeserializer$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testadapter.TaskDefSerializers$TaskDefSerializer$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testadapter.TaskInfo"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testadapter.TaskInfo$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testadapter.TaskInfo$Deserializer$")
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
      // internal, not an issue.
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testinterface.internal.BridgeBase"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testinterface.internal.EventSerializer"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testinterface.internal.EventSerializer$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testinterface.internal.FingerprintSerializer"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testinterface.internal.FingerprintSerializer$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testinterface.internal.FingerprintSerializer$DeserializedAnnotatedFingerprint"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testinterface.internal.FingerprintSerializer$DeserializedSubclassFingerprint"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testinterface.internal.SelectorSerializer"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testinterface.internal.SelectorSerializer$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testinterface.internal.TaskDefSerializer"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testinterface.internal.TaskDefSerializer$"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.testinterface.TestDetector#RawDefinitions.definedTests"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testinterface.internal.ThrowableSerializer"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testinterface.internal.ThrowableSerializer$"),
      ProblemFilters.exclude[MissingTypesProblem](
          "org.scalajs.testinterface.internal.Master"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.testinterface.internal.Master.handleMsgImpl"),
      ProblemFilters.exclude[MissingTypesProblem](
          "org.scalajs.testinterface.internal.Slave"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.testinterface.internal.Slave.handleMsgImpl")
  )
}
