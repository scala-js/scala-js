import sbt._
import Keys._

import scala.scalajs.sbtplugin._
import ScalaJSPlugin._
import ScalaJSKeys._
import Implicits._

import scala.scalajs.tools.env._
import scala.scalajs.tools.io._

import org.eclipse.jetty.server._
import org.eclipse.jetty.server.handler._
import org.eclipse.jetty.util.component._

import java.io.File

object Jetty9Test {

  private val jettyPort = 23548

  val runSetting = run in fastOptStage <<= Def.inputTask {
    val env = (jsEnv in fastOptStage in Compile).value.asInstanceOf[ComJSEnv]
    val cp = (scalaJSExecClasspath in fastOptStage in Compile).value
    val jsConsole = scalaJSConsole.value

    val code = new MemVirtualJSFile("runner.js").withContent(
      """
      scalajsCom.init(function(msg) {
        jQuery.ajax({
          url: msg,
          success: function(dat) {
            scalajsCom.send(dat.trim());
            scalajsCom.close();
          },
          error: function() {
            scalajsCom.send("failed!");
            scalajsCom.close();
          }
        });
      });
      """
    )

    val runner = env.comRunner(cp, code, streams.value.log, jsConsole)

    runner.start()

    val jetty = setupJetty((resourceDirectory in Compile).value)

    jetty.addLifeCycleListener(new AbstractLifeCycle.AbstractLifeCycleListener {
      override def lifeCycleStarted(event: LifeCycle): Unit = {
        try {
          runner.send(s"http://localhost:$jettyPort/test.txt")
          val msg = runner.receive()
          val expected = "It works!"
          if (msg != expected)
            sys.error(s"""received "$msg" instead of "$expected"""")
        } finally {
          runner.close()
          jetty.stop()
        }
      }
    })

    jetty.start()
    runner.await()
    jetty.join()
  }

  private def setupJetty(dir: File): Server = {
    val server = new Server(jettyPort)

    val resource_handler = new ResourceHandler()
    resource_handler.setResourceBase(dir.getAbsolutePath)

    val handlers = new HandlerList()
    handlers.setHandlers(Array(resource_handler, new DefaultHandler()))
    server.setHandler(handlers)

    server
  }

}
