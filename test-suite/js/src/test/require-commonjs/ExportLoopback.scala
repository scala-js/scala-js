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
  val exportsNamespace: Future[js.Dynamic] = {
    js.Promise.resolve[Unit](())
      .`then`[js.Dynamic](_ => js.Dynamic.global.require("./main.js"))
      .toFuture
  }
}
