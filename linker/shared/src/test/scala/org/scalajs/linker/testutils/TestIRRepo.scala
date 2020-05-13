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
import scala.concurrent.ExecutionContext.Implicits.global

import org.scalajs.linker.StandardImpl
import org.scalajs.linker.interface.IRFile

object TestIRRepo {
  val minilib: Future[Seq[IRFile]] = load(StdlibHolder.minilib)
  val fulllib: Future[Seq[IRFile]] = load(StdlibHolder.fulllib)
  val empty: Future[Seq[IRFile]] = Future.successful(Nil)

  private def load(stdlibPath: String) = {
    val globalIRCache = StandardImpl.irFileCache()
    Platform.loadJar(stdlibPath)
      .flatMap(globalIRCache.newCache.cached _)
  }
}
