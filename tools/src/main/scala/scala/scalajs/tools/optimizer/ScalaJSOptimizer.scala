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

import java.net.URI

import org.json.simple.JSONValue

import scala.scalajs.ir

import scala.scalajs.tools.logging._
import scala.scalajs.tools.io._
import scala.scalajs.tools.classpath._
import scala.scalajs.tools.sourcemap._
import scala.scalajs.tools.json._

import OptData._

import ScalaJSPackedClasspath.{ writePackInfo, PackInfoData }

/** Scala.js optimizer: does type-aware global dce. */
class ScalaJSOptimizer {
  import ScalaJSOptimizer._

  private[this] var logger: Logger = _
  private[this] val encodedNameToIRFile =
    mutable.Map.empty[String, VirtualScalaJSIRFile]

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
    val userData = classpath.irFiles map { irFile =>
      val data = readData(irFile.info)
      val encodedName = data.encodedName
      encodedNameToIRFile += encodedName -> irFile
      data
    }
    new Analyzer(logger, coreData ++ userData)
  }

  private def readData(infoFile: String): ClassInfoData =
    fromJSON[ClassInfoData](JSONValue.parse(infoFile))

  private def readData(info: ir.Trees.Tree): ClassInfoData = {
    // TODO Definitely not the most efficient way to do this
    import ir.Trees._
    import scala.collection.JavaConverters._

    def toJSONValue(tree: Tree): Any = {
      (tree: @unchecked) match {
        case Null()                  => null
        case BooleanLiteral(value)   => value
        case IntLiteral(value)       => value
        case DoubleLiteral(value)    => value
        case StringLiteral(value, _) => value
        case JSArrayConstr(items)    => items.map(toJSONValue).asJava
        case JSObjectConstr(fields)  =>
          fields.map(x => (x._1.name, toJSONValue(x._2))).toMap.asJava
      }
    }
    fromJSON[ClassInfoData](toJSONValue(info).asInstanceOf[AnyRef])
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
      irFile <- encodedNameToIRFile.get(classInfo.encodedName)
    } {
      import ir.Trees._
      import ir.{ScalaJSClassEmitter => Emitter}

      val classDef = irFile.tree

      def addTree(tree: Tree): Unit =
        builder.addIRTree(tree)

      if (classInfo.isImplClass) {
        for (m @ MethodDef(Ident(encodedName, _), _, _, _) <- classDef.defs) {
          if (classInfo.methodInfos(encodedName).isReachable)
            addTree(Emitter.genTraitImplMethod(classDef, m))
        }
      } else if (!classInfo.hasInstantiation) {
        // there is only the data anyway
        addTree(classDef)
      } else {
        val kind = classDef.kind
        assert(kind.isClass)
        if (classInfo.isAnySubclassInstantiated) {
          addTree(Emitter.genConstructor(classDef))
          classDef.defs foreach {
            case m @ MethodDef(Ident(encodedName, _), _, _, _) =>
              if (classInfo.methodInfos(encodedName).isReachable)
                addTree(Emitter.genMethod(classDef, m))
            case m: MethodDef =>
              addTree(Emitter.genMethod(classDef, m))
            case p: PropertyDef =>
              addTree(Emitter.genProperty(classDef, p))
            case _ =>
          }
        }
        if (classInfo.isDataAccessed) {
          addTree(Emitter.genInstanceTests(classDef))
          addTree(Emitter.genArrayInstanceTests(classDef))
          addTree(Emitter.genTypeData(classDef))
        }
        if (classInfo.isAnySubclassInstantiated)
          addTree(Emitter.genSetTypeData(classDef))
        if (classInfo.isModuleAccessed)
          addTree(Emitter.genModuleAccessor(classDef))
        addTree(Emitter.genClassExports(classDef))
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
  private final val StateData = 2
  private final val StateModuleAccessor = 3
  private final val StateExports = 4
}
