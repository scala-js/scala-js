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

import org.scalajs.ir.Trees.ClassDef

import org.scalajs.logging._

import org.scalajs.linker._
import org.scalajs.linker.interface._

object LinkingUtils {
  def testLink(classDefs: Seq[ClassDef],
      moduleInitializers: List[ModuleInitializer])(
      implicit ec: ExecutionContext): Future[Unit] = {

    val linker = StandardImpl.linker(StandardConfig())
    val classDefsFiles = classDefs.map(MemClassDefIRFile(_))
    val output = LinkerOutput(MemOutputFile())

    TestIRRepo.minilib.flatMap { stdLibFiles =>
      linker.link(stdLibFiles ++ classDefsFiles, moduleInitializers,
          output, new ScalaConsoleLogger(Level.Error))
    }
  }
}
