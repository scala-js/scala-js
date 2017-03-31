/*                     __                                                   *\
**     ________ ___   / /  ___      __ ____  PhantomJS support for Scala.js **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2017, LAMP/EPFL       **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    https://www.scala-js.org/      **
** /____/\___/_/ |_/____/_/ | |__/ /____/                                   **
**                          |/____/                                         **
\*                                                                          */

package org.scalajs.jsenv.phantomjs

private[phantomjs] trait WebsocketListener {
  def onRunning(): Unit
  def onOpen(): Unit
  def onClose(): Unit
  def onMessage(msg: String): Unit

  def log(msg: String): Unit
}
