package build

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
    // private[sbtplugin], not an issue.
    ProblemFilters.exclude[MissingClassProblem](
        "org.scalajs.sbtplugin.FrameworkDetector"),
    ProblemFilters.exclude[DirectMissingMethodProblem](
        "org.scalajs.sbtplugin.FrameworkDetector.detect"),
    ProblemFilters.exclude[DirectMissingMethodProblem](
        "org.scalajs.sbtplugin.FrameworkDetector.this"),

    // private, not an issue.
    ProblemFilters.exclude[MissingClassProblem](
        "org.scalajs.sbtplugin.FrameworkDetector$StoreConsole")
  )

  val TestCommon = Seq(
      // private[scalajs], not an issue.
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.testcommon.FrameworkInfo.name"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.testcommon.FrameworkInfo.this"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.testcommon.RunnerArgs.this"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testcommon.JSMasterEndpoints"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testcommon.JSMasterEndpoints$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testcommon.JSSlaveEndpoints"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testcommon.JSSlaveEndpoints$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testcommon.JVMMasterEndpoints"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testcommon.JVMMasterEndpoints$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testcommon.JVMSlaveEndpoints"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testcommon.JVMSlaveEndpoints$"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.testcommon.RPCCore#lambda#@tach#1.this"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.testcommon.RPCCore.close"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.testcommon.RPCCore.this")
  )

  val TestAdapter = TestCommon ++ Seq(
      // breaking in theory
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testadapter.package"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testadapter.package$"),

      // private[testadapter], not an issue.
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.testadapter.package.VMTermTimeout"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.testadapter.ScalaJSRunner.this"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.testadapter.ScalaJSTask.this"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.testadapter.ScalaJSFramework.runDone"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.testadapter.ScalaJSFramework.frameworkName"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.testadapter.ScalaJSFramework.optionalExportsNamespacePrefix"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.testadapter.ScalaJSFramework.moduleKind"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.testadapter.ScalaJSFramework.moduleIdentifier"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.testadapter.ScalaJSFramework.libEnv"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.testadapter.ScalaJSFramework.jsConsole"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.testadapter.ScalaJSFramework.logger"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.testadapter.ScalaJSRunner.getSlave"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.testadapter.ScalaJSRunner.loggerLock"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.testadapter.ComJSEnvRPC.close"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.testadapter.ComJSEnvRPC.this"),

      // private, not an issue.
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testadapter.ScalaJSRunner$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testadapter.ScalaJSRunner$RichFuture"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testadapter.ScalaJSRunner$RichFuture$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testadapter.ScalaJSTask$LogElement")
  )

  val CLI = Seq(
  )

  val Library = Seq(
  )

  val TestInterface = TestCommon ++ Seq(
      // internal, not an issue.
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testinterface.internal.Com"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testinterface.internal.Com$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testinterface.internal.FrameworkDetector"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testinterface.internal.FrameworkDetector$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testinterface.internal.InfoSender"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testinterface.internal.Master"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testinterface.internal.Master$lambda$1"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testinterface.internal.Master$lambda$2"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testinterface.internal.Master$lambda$3"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testinterface.internal.Master$lambda$4"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testinterface.internal.Slave"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testinterface.internal.Slave$Invalidatable"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testinterface.internal.Slave$RemoteEventHandler"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testinterface.internal.Slave$RemoteLogger"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testinterface.internal.Slave$lambda$1"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testinterface.internal.Slave$lambda$2"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testinterface.internal.Slave$lambda$3"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.testinterface.internal.Slave$lambda$4"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.testinterface.internal.JSRPC.close"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.testinterface.internal.JSRPC#Com.close")
  )
}
