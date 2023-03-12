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
  private val globalIRCache = StandardImpl.irFileCache()

  val minilib: Future[Seq[IRFile]] = load(StdlibHolder.minilib)
  val javalib: Future[Seq[IRFile]] = load(StdlibHolder.javalib)
  val empty: Future[Seq[IRFile]] = Future.successful(Nil)
  val previousLibs: Map[String, Future[Seq[IRFile]]] =
    StdlibHolder.previousLibs.map(x => x._1 -> load(x._2))

  private def load(stdlibPath: String) = {
    Platform.loadJar(stdlibPath)
      .flatMap(globalIRCache.newCache.cached _)
  }
}
