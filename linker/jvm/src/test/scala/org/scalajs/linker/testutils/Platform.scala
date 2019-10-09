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

package org.scalajs.linker.testutils

import scala.concurrent._

import java.nio.file.Paths

import org.scalajs.linker.PathIRContainer
import org.scalajs.linker.interface.IRContainer

object Platform {
  def loadJar(path: String)(implicit ec: ExecutionContext): Future[Seq[IRContainer]] =
    PathIRContainer.fromClasspath(Seq(Paths.get(path))).map(_._1)
}
