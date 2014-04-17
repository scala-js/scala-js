/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.tools.optimizer

import scala.annotation.{switch, tailrec}

import scala.collection.mutable

import net.liftweb.json._

import java.net.URI

import scala.scalajs.tools.logging._
import scala.scalajs.tools.io._
import scala.scalajs.tools.classpath._
import scala.scalajs.tools.sourcemap._
import OptData._

import ScalaJSPackedClasspath.{ writePackInfo, PackInfoData }

/** Scala.js optimizer: does type-aware global dce. */
class ScalaJSOptimizer {
  import ScalaJSOptimizer._

  private[this] var logger: Logger = _
  private[this] val encodedNameToClassfile =
    mutable.Map.empty[String, VirtualScalaJSClassfile]

  /** Applies Scala.js-specific optimizations to a sequence of .js files.
   *  See [[ScalaJSOptimizer.Inputs]] for details about the required and
   *  optional inputs.
   *  See [[ScalaJSOptimizer.OutputConfig]] for details about the configuration
   *  for the output of this method.
   *  Returns a [[ScalaJSOptimizer.Result]] containing the result of the
   *  optimizations. Its output file will contain, in that order:
   *  1. The Scala.js core lib,
   *  2. The result of dead code elimination applied to Scala.js class files,
   *  3. The custom .js files, in the same order as they were listed in inputs.
   */
  def optimize(inputs: Inputs, outputConfig: OutputConfig,
      logger: Logger): Unit = {
    this.logger = logger
    try {
      val analyzer = parseInfoFiles(inputs.classpath)
      analyzer.computeReachability(inputs.manuallyReachable)
      writeDCEedOutput(inputs, outputConfig, analyzer)

      // Write out pack order (constant: file is stand alone)
      writePackInfo(outputConfig.writer, PackInfoData(packOrder = 0))
    } finally {
      this.logger = null
    }
  }

  private def parseInfoFiles(classpath: ScalaJSClasspath): Analyzer = {
    val coreData = classpath.coreInfoFiles.map(f => readData(f.content))
    val userData = classpath.classFiles map { classfile =>
      val data = readData(classfile.info)
      val encodedName = data.encodedName
      encodedNameToClassfile += encodedName -> classfile
      data
    }
    new Analyzer(logger, coreData ++ userData)
  }

  private def readData(infoFile: String): ClassInfoData = {
    implicit val formats = DefaultFormats
    Extraction.extract[ClassInfoData](JsonParser.parse(infoFile))
  }

  private def writeDCEedOutput(inputs: Inputs, outputConfig: OutputConfig,
      analyzer: Analyzer): Unit = {

    val builder = {
      import outputConfig._
      if (wantSourceMap)
        new JSFileBuilderWithSourceMap(name,
            writer.contentWriter,
            writer.sourceMapWriter,
            relativizeSourceMapBase)
      else
        new JSFileBuilder(name, writer.contentWriter)
    }

    builder.addFile(inputs.classpath.coreJSLibFile)

    def compareClassInfo(lhs: analyzer.ClassInfo, rhs: analyzer.ClassInfo) = {
      if (lhs.ancestorCount != rhs.ancestorCount) lhs.ancestorCount < rhs.ancestorCount
      else lhs.encodedName.compareTo(rhs.encodedName) < 0
    }

    for {
      classInfo <- analyzer.classInfos.values.toSeq.sortWith(compareClassInfo)
      if classInfo.isNeededAtAll
      classfile <- encodedNameToClassfile.get(classInfo.encodedName)
    } {
      val className = classInfo.encodedName

      class NoSourceMapLinkSelector extends (String => Boolean) {
        override def apply(line: String): Boolean =
          !line.startsWith("//@ sourceMappingURL=")
      }

      class ReachableMethodsSelector extends NoSourceMapLinkSelector {
        private[this] val methodLinePrefix =
          if (classInfo.isImplClass) "ScalaJS.impls"
          else s"ScalaJS.c.$className.prototype"

        private[this] val prefixLength = methodLinePrefix.length
        private[this] var inReachableMethod = false

        override def apply(line: String): Boolean = {
          super.apply(line) && {
            if (line.startsWith(methodLinePrefix)) {
              // Update inReachableMethod
              if (line(prefixLength) == '.') {
                val name = line.substring(prefixLength+1).takeWhile(_ != ' ')
                val encodedName = parse('"'+name+'"').asInstanceOf[JString].s // see #330
                inReachableMethod = classInfo.methodInfos(encodedName).isReachable
              } else {
                // this is an exported method with []-select
                inReachableMethod = true
              }
            }
            inReachableMethod
          }
        }
      }

      class FullInfoSelector extends ReachableMethodsSelector {
        private[this] var state: Int = StateConstructor

        @tailrec
        override final def apply(line: String): Boolean = {
          if (line.startsWith("//@ sourceMappingURL=")) false
          else {
            (state: @switch) match {
              case StateConstructor =>
                if (line.startsWith(s"ScalaJS.c.$className.prototype.constructor =")) {
                  // last line of the Constructor part, switch for next time
                  state = StateMethods
                }
                // include if any subclass is instantiated
                classInfo.isAnySubclassInstantiated

              case StateMethods =>
                if (line.startsWith("/** @constructor */")) {
                  // beginning of the InheritableConstructor part, switch and redo
                  state = StateInheritableConstructor
                  apply(line)
                } else {
                  // still in the Methods part, use the super apply
                  classInfo.isAnySubclassInstantiated && super.apply(line)
                }

              case StateInheritableConstructor =>
                if (line.startsWith("ScalaJS.is.")) {
                  // beginning of the Data part, switch and redo
                  state = StateData
                  apply(line)
                } else {
                  // inheritable constructor
                  classInfo.isAnySubclassInstantiated
                }

              case StateData =>
                if (line.startsWith("ScalaJS.c.")) {
                  // last line of the Data part which is the set-classdata stat
                  state = StateModuleAccessor
                  classInfo.isAnySubclassInstantiated
                } else {
                  // in the Data part
                  classInfo.isDataAccessed
                }

              case StateModuleAccessor =>
                classInfo.isModuleAccessed
            }
          }
        }
      }

      val lines = classfile.readLines().filterNot(_.startsWith("//@"))
      if (classInfo.isImplClass) {
        val selector = new ReachableMethodsSelector
        builder.addPartsOfFile(classfile)(selector)
      } else if (!classInfo.hasInstantiation) {
        // there is only the data anyway
        builder.addFile(classfile)
      } else {
        val selector = new FullInfoSelector
        builder.addPartsOfFile(classfile)(selector)
      }
    }

    for (file <- inputs.customScripts)
      builder.addFile(file)

    builder.complete()
  }
}

object ScalaJSOptimizer {
  /** Inputs of the Scala.js optimizer. */
  final case class Inputs(
      /** The Scala.js classpath entries. */
      classpath: ScalaJSClasspath,
      /** Additional scripts to be appended in the output. */
      customScripts: Seq[VirtualJSFile] = Nil,
      /** Manual additions to reachability */
      manuallyReachable: Seq[ManualReachability] = Nil
  )

  sealed abstract class ManualReachability
  final case class ReachObject(name: String) extends ManualReachability
  final case class Instantiate(name: String) extends ManualReachability
  final case class ReachMethod(className: String, methodName: String,
      static: Boolean) extends ManualReachability

  /** Configuration for the output of the Scala.js optimizer. */
  final case class OutputConfig(
      /** Name of the output file. (used to refer to sourcemaps) */
      name: String,
      /** Writer for the output. */
      writer: VirtualScalaJSPackfileWriter,
      /** Ask to produce source map for the output */
      wantSourceMap: Boolean = false,
      /** Base path to relativize paths in the source map. */
      relativizeSourceMapBase: Option[URI] = None
  )

  private final val StateConstructor = 0
  private final val StateMethods = 1
  private final val StateInheritableConstructor = 2
  private final val StateData = 3
  private final val StateModuleAccessor = 4
}
