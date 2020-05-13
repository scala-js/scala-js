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

import org.scalajs.linker.StandardImpl
import org.scalajs.linker.frontend.IRLoader
import org.scalajs.linker.interface.IRFile

object TestIRRepo {
  val minilib = new TestIRRepo(StdlibHolder.minilib)
  val fulllib = new TestIRRepo(StdlibHolder.fulllib)
}

final class TestIRRepo(stdlibPath: String) {
  import scala.concurrent.ExecutionContext.Implicits.global

  private val globalIRCache = StandardImpl.irFileCache()

  val stdlibIRFiles: Future[Seq[IRFile]] = {
    Platform.loadJar(stdlibPath)
      .flatMap(globalIRCache.newCache.cached _)
  }

  lazy val irLoader: Future[IRLoader] =
    stdlibIRFiles.flatMap((new IRLoader).update(_))
}
