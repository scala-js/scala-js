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

package org.scalajs.jsenv.test

import org.scalajs.jsenv._

class StoreJSConsole extends JSConsole {
  private[this] val buf = new StringBuilder()

  def log(msg: Any): Unit = {
    buf.append(msg.toString)
    buf.append('\n')
  }

  def getLog: String = buf.toString
}
