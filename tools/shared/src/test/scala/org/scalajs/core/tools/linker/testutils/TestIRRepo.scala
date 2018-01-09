package org.scalajs.core.tools.linker.testutils

import scala.collection.mutable

import org.scalajs.core.tools.io._

import org.scalajs.core.tools.linker.analyzer.Infos._
import org.scalajs.core.tools.linker.irio._

object TestIRRepo {
  private val stdlibPath =
    System.getProperty("org.scalajs.core.tools.linker.stdlibjar")

  private val globalIRCache = new IRFileCache

  val stdlibIRFiles: Seq[VirtualScalaJSIRFile with RelativeVirtualFile] =
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
