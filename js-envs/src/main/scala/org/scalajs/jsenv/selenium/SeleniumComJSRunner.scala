package org.scalajs.jsenv.selenium

import java.util.concurrent.TimeoutException

import org.scalajs.core.tools.io.{MemVirtualJSFile, VirtualJSFile}
import org.scalajs.core.tools.jsdep.ResolvedJSDependency
import org.scalajs.jsenv._

import scala.annotation.tailrec
import scala.concurrent.Await
import scala.concurrent.duration.Duration

class SeleniumComJSRunner(browserProvider: SeleniumBrowser,
    libs: Seq[ResolvedJSDependency], code: VirtualJSFile, keepAlive: Boolean)
    extends SeleniumAsyncJSRunner(browserProvider, libs, code, keepAlive)
    with ComJSRunner {

  protected def envName: String = "SeleniumComJSRunner on " + browserProvider.name

  def send(msg: String): Unit = {
    awaitForBrowser()
    browser.getWebDriver.executeScript(
        "this.scalajsSeleniumComJSRunnerChannel.recvMessage(arguments[0])", msg)
    browser.processConsoleLogs(console)
  }

  def receive(timeout: Duration): String = {
    awaitForBrowser(timeout)
    @tailrec def loop(): String = {
      val script = "return this.scalajsSeleniumComJSRunnerChannel.popOutMsg();"
      browser.getWebDriver.executeScript(script) match {
        case null =>
          loop()

        case msg: String =>
          browser.processConsoleLogs(console)
          msg

        case obj =>
          browser.processConsoleLogs(console)
          throw new IllegalStateException("Receive ill formed message of type " +
              obj.getClass + " with value: " + obj)
      }
    }
    loop()
  }

  def close(): Unit = {
    browser.processConsoleLogs(console)
    if (!keepAlive)
      browser.close()
  }

  override protected def initFiles(): Seq[VirtualJSFile] =
    super.initFiles() :+ comSetupFile()

  protected def comSetupFile(): VirtualJSFile = {
    val code = {
      s"""
         |(function() {
         |  // Buffers for outgoing messages
         |  var sendMsgBufIn = []; // push to this one
         |  var sendMsgBufOut = []; // pop from this one
         |
         |  var onReceive = null;
         |
         |  this.scalajsCom = {
         |    init: function(recvCB) {
         |      onReceive = recvCB;
         |    },
         |    send: function(msg) {
         |      sendMsgBufIn.push(msg);
         |    },
         |    close: function() {
         |      if (sendMsgBufIn.length + sendMsgBuf2.length != 0) {
         |        this.setTimeout(this.scalajsCom.close, 10);
         |      } else {
         |        seleniumAsyncJSRunnerClose = true;
         |      }
         |    }
         |  }
         |
         |  this.scalajsSeleniumComJSRunnerChannel = {
         |    popOutMsg: function() {
         |      if (sendMsgBufOut.length == 0) {
         |         sendMsgBufOut = sendMsgBufIn.reverse();
         |         sendMsgBufIn = [];
         |      }
         |      return sendMsgBufOut.pop();
         |    },
         |    recvMessage: function(msg) {
         |      onReceive(msg);
         |    }
         |  }
         |})();
      """.stripMargin
    }
    new MemVirtualJSFile("comSetup.js").withContent(code)
  }

  protected def awaitForBrowser(timeout: Duration = Duration.Inf): Unit = {
    if (!Await.ready(future, timeout).isCompleted)
      throw new TimeoutException()
  }
}
