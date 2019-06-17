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

import org.scalajs.linker._
import org.scalajs.linker.standard._
import org.scalajs.linker.analyzer.Infos._

object TestIRRepo {
  val minilib = new TestIRRepo(StdlibHolder.minilib)
  val fulllib = new TestIRRepo(StdlibHolder.fulllib)

  class InfoLoader(encodedNameToFile: Map[String, IRFileImpl]) {
    private val infosCache = mutable.Map.empty[String, Future[ClassInfo]]

    def loadInfo(encodedName: String)(
        implicit ec: ExecutionContext): Option[Future[ClassInfo]] = {
      infosCache.synchronized {
        infosCache.get(encodedName).orElse {
          val info =
            encodedNameToFile.get(encodedName).map(_.tree.map(generateClassInfo))
          info.foreach(i => infosCache.put(encodedName, i))
          info
        }
      }
    }
  }
}

final class TestIRRepo(stdlibPath: String) {
  import scala.concurrent.ExecutionContext.Implicits.global
  import TestIRRepo.InfoLoader

  private val globalIRCache = IRFileCache()

  val stdlibIRFiles: Future[Seq[IRFile]] = {
    Platform.loadJar(stdlibPath)
      .flatMap(globalIRCache.newCache.cached _)
  }

  lazy val loader: Future[InfoLoader] = {
    def toElem(f: IRFile) = {
      val impl = IRFileImpl.fromIRFile(f)
      impl.entryPointsInfo.map(i => i.encodedName -> impl)
    }

    for {
      files <- stdlibIRFiles
      encodedNameToFile <- Future.traverse(files)(toElem)
    } yield {
      new InfoLoader(encodedNameToFile.toMap)
    }
  }
}
