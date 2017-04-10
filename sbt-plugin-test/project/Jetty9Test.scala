import sbt._
import Keys._

import org.scalajs.sbtplugin._
import ScalaJSPlugin.autoImport._
import Implicits._

import org.scalajs.jsenv._
import org.scalajs.core.tools.io._

import org.eclipse.jetty.server._
import org.eclipse.jetty.server.handler._
import org.eclipse.jetty.util.component._

import java.io.File

import scala.concurrent.duration._

object Jetty9Test {

  private val jettyPort = 23548

  val runSetting = run <<= Def.inputTask {
    val jsEnv = (loadedJSEnv in Compile).value.asInstanceOf[ComJSEnv]
    val jsConsole = scalaJSConsole.value

    val code = new MemVirtualJSFile("runner.js").withContent(
      """
      scalajsCom.init(function(msg) {
        var xhr = new XMLHttpRequest();
        xhr.open("GET", msg);
        xhr.onload = (function() {
          scalajsCom.send(xhr.responseText.trim());
          scalajsCom.close();
        });
        xhr.onerror = (function() {
          scalajsCom.send("failed!");
          scalajsCom.close();
        });
        xhr.send();
      });
      """
    )

    val runner = jsEnv.comRunner(code :: Nil)

    runner.start(streams.value.log, jsConsole)

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
    runner.await(30.seconds)
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
