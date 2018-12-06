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

import org.scalajs.io._

import org.scalajs.linker.analyzer.Infos._
import org.scalajs.linker.irio._

object TestIRRepo {
  private val stdlibPath = MinilibHolder.minilib

  private val globalIRCache = new IRFileCache

  val stdlibIRFiles: Seq[VirtualScalaJSIRFile] =
    globalIRCache.newCache.cached(Seq(Platform.loadJar(stdlibPath)))

  private val stdlibEncodedNameToFile =
    stdlibIRFiles.map(f => f.entryPointsInfo.encodedName -> f).toMap

  private val infosCache = mutable.Map.empty[String, ClassInfo]

  def loadInfo(encodedName: String): Option[ClassInfo] = {
    infosCache.synchronized {
      infosCache.get(encodedName).orElse {
        stdlibEncodedNameToFile.get(encodedName).map { f =>
          generateClassInfo(f.tree)
        }
      }
    }
  }
}
