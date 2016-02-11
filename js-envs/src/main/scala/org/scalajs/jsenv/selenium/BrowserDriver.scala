package org.scalajs.jsenv.selenium

import org.openqa.selenium.remote.RemoteWebDriver
import org.scalajs.jsenv.JSConsole

trait BrowserDriver {

  private var webDriver: RemoteWebDriver = _

  def getWebDriver: RemoteWebDriver = synchronized {
    if (webDriver == null)
      throw BrowserDriver.BrowserNotOpenException
    webDriver
  }

  /** Starts a new instance of the browser. */
  def start(): Unit = synchronized {
    assert(!isOpened, "start() may only start one instance at a time.")
    webDriver = newDriver()
  }

  /** Closes the instance of the browser. */
  def close(): Unit = synchronized {
    if (isOpened) {
      webDriver.close()
      webDriver = null
    }
  }

  def isOpened: Boolean = synchronized {
    webDriver != null
  }

  /** Tries to get the console logs from the browser and prints them on the
   *  JSConsole. This method never throws BrowserNotOpenException.
   */
  def processConsoleLogs(console: JSConsole): Unit = {
    try {
      newConsoleLogsIterator().foreach(console.log)
    } catch {
      case BrowserDriver.BrowserNotOpenException => // Do nothing
    }
  }

  protected def newConsoleLogsIterator(): Iterator[String]

  protected def newDriver(): RemoteWebDriver
}

object BrowserDriver {
  object BrowserNotOpenException extends Throwable
}
