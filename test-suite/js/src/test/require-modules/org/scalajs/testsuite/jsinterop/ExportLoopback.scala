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
import scala.scalajs.js.annotation._

import scala.concurrent.Future

object ExportLoopback {
  val exportsNamespace: Future[js.Dynamic] =
    js.dynamicImport(mainModule).toFuture

  @js.native
  @JSImport("./main.js", JSImport.Namespace)
  private val mainModule: js.Dynamic = js.native
}
