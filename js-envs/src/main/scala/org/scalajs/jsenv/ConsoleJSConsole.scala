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

package org.scalajs.jsenv

/** A JS console that prints on the console */
object ConsoleJSConsole extends JSConsole {
  override def log(msg: Any): Unit = {
    Console.println(msg)
  }
}
