/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.jsenv.phantomjs

private[phantomjs] trait WebsocketManager {
  def start(): Unit
  def stop(): Unit
  def sendMessage(msg: String): Unit
  def localPort: Int
  def isConnected: Boolean
  def isClosed: Boolean
}
