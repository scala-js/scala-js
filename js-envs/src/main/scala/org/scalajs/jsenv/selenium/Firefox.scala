package org.scalajs.jsenv.selenium

import java.util

import org.openqa.selenium.remote.RemoteWebDriver
import org.scalajs.core.tools.io.{MemVirtualJSFile, VirtualJSFile}

import scala.collection.JavaConversions._

object Firefox extends SeleniumBrowser {
  def name: String = "Firefox"

  def newDriver: BrowserDriver = new FirefoxDriver

  override def initFiles(): Seq[VirtualJSFile] =
    hijackConsole() +: super.initFiles()

  /** The default selenium console output is broken, this is the workaround. */
  private def hijackConsole(): VirtualJSFile = {
    new MemVirtualJSFile("hijackConsole.js").withContent(
      """
        |(function () {
        |  var console_log_hijack = [];
        |  var oldLog = console.log;
        |  console.log = function (msg) {
        |    console_log_hijack.push(msg);
        |    oldLog.apply(console, arguments);
        |  };
        |  this.scalajsPopHijackedConsoleLog = function () {
        |    var log = console_log_hijack;
        |    if (log.length != 0)
        |      console_log_hijack = [];
        |    return log;
        |  };
        |})();
      """.stripMargin
    )
  }

  private class FirefoxDriver extends BrowserDriver {
    protected def newDriver(): RemoteWebDriver =
      new org.openqa.selenium.firefox.FirefoxDriver()

    def newConsoleLogsIterator(): Iterator[String] = {
      getWebDriver.executeScript("return scalajsPopHijackedConsoleLog();") match {
        case log: util.ArrayList[_] =>
          log.iterator().map(_.toString)

        case _ =>
          throw new Exception("Ill formatted message.")
      }
    }
  }
}
