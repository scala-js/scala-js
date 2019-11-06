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

import scala.collection.mutable
import scala.concurrent._

import org.scalajs.ir.Names.ClassName

import org.scalajs.linker.StandardImpl
import org.scalajs.linker.interface.IRFile
import org.scalajs.linker.interface.unstable.IRFileImpl
import org.scalajs.linker.analyzer.Infos._

object TestIRRepo {
  val minilib = new TestIRRepo(StdlibHolder.minilib)
  val fulllib = new TestIRRepo(StdlibHolder.fulllib)

  class InfoLoader(classNameToFile: Map[ClassName, IRFileImpl]) {
    private val infosCache = mutable.Map.empty[ClassName, Future[ClassInfo]]

    def loadInfo(className: ClassName)(
        implicit ec: ExecutionContext): Option[Future[ClassInfo]] = {
      infosCache.synchronized {
        infosCache.get(className).orElse {
          val info =
            classNameToFile.get(className).map(_.tree.map(generateClassInfo))
          info.foreach(i => infosCache.put(className, i))
          info
        }
      }
    }
  }
}

final class TestIRRepo(stdlibPath: String) {
  import scala.concurrent.ExecutionContext.Implicits.global
  import TestIRRepo.InfoLoader

  private val globalIRCache = StandardImpl.irFileCache()

  val stdlibIRFiles: Future[Seq[IRFile]] = {
    Platform.loadJar(stdlibPath)
      .flatMap(globalIRCache.newCache.cached _)
  }

  lazy val loader: Future[InfoLoader] = {
    def toElem(f: IRFile) = {
      val impl = IRFileImpl.fromIRFile(f)
      impl.entryPointsInfo.map(i => i.className -> impl)
    }

    for {
      files <- stdlibIRFiles
      classNameToFile <- Future.traverse(files)(toElem)
    } yield {
      new InfoLoader(classNameToFile.toMap)
    }
  }
}
