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

import org.scalajs.core.tools.logging.Logger

trait JSRunner {
  /** Run the associated JS code. Throw if an error occurs. */
  def run(logger: Logger, console: JSConsole): Unit
}
