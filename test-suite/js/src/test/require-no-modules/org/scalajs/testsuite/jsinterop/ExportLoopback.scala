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

package org.scalajs.testsuite.jsinterop

import scala.scalajs.js

import scala.concurrent.Future

object ExportLoopback {
  def exportsNamespace: Future[js.Dynamic] =
    throw new AssertionError("attempted to get exportsNamsepace in NoModule mode")
}
