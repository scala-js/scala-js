package scala.scalajs.sbtplugin.optimizer

import scala.annotation.tailrec

import scala.collection.mutable

import sbt.{File => _, _}
import net.liftweb.json._

import scala.scalajs.sbtplugin.FileSystem
import scala.scalajs.sbtplugin.Utils._
import OptData._

/** Scala.js optimizer: does type-aware global dce. */
class ScalaJSOptimizer[FS <: FileSystem](val fs: FS, logger: Logger) {
  import ScalaJSOptimizer._
  import fs.{File, fileOps}

  val encodedNameToJSFile = mutable.Map.empty[String, File]

  /** Applies Scala.js-specific optimizations to a sequence of .js files.
   *  The inputs must contain exactly one file named "scalajs-corejslib.js"
   *  ([[scala.scalajs.sbtplugin.Utils.CoreJSLibFileName]]), which must be the
   *  Scala.js core JS library.
   *  In the directory containing the Scala.js core JS library, the files
   *  "javalangObject.sjsinfo" and "javalangString.sjsinfo" must also be
   *  present (but not listed in the inputs).
   *  The inputs otherwise contain a mix of so-called Scala.js class files,
   *  i.e., .js files emitted by Scala.js, which have a sibling with extension
   *  ".sjsinfo", and other custom .js files.
   *  The Scala.js class files will be processed by this method to eliminate
   *  dead code. The output file will contain, in that order:
   *  1. The Scala.js core lib,
   *  2. The result of dead code elimination applied to Scala.js class files,
   *  3. The custom .js files, in the same order as they were listed in inputs.
   */
  def optimize(inputs: Seq[File], output: File,
      relativeSourceMaps: Boolean = false): Unit = {

    val (classFiles, otherFiles) = inputs.partition(isScalaJSClassFile)

    val (coreJSLibFiles, customScripts) =
      otherFiles.partition(_.name == CoreJSLibFileName)
    if (coreJSLibFiles.size != 1)
      throw new IllegalArgumentException(
          s"There must be exactly one Scala.js core library ($CoreJSLibFileName) in the inputs.")
    val coreJSLibFile = coreJSLibFiles.head

    val infoFiles0 = classFiles.map(jsFileToInfo)
    val infoFiles = Seq(
        coreJSLibFile.withName("javalangObject.sjsinfo"),
        coreJSLibFile.withName("javalangString.sjsinfo")) ++ infoFiles0

    val analyzer = parseInfoFiles(infoFiles)
    analyzer.computeReachability()
    writeDCEedOutput(analyzer, output, coreJSLibFile, customScripts)
  }

  private def isScalaJSClassFile(file: File): Boolean =
    file.hasExtension(".js") && jsFileToInfo(file).exists

  private def jsFileToInfo(file: File): File =
    file.withExtension(".js", ".sjsinfo")

  private def parseInfoFiles(infoFiles: Seq[File]): Analyzer = {
    val allData = infoFiles map { infoFile =>
      val data = readData(infoFile)
      val encodedName = data.encodedName
      val jsFile = infoFile.withExtension(".sjsinfo", ".js")
      if (jsFile.exists)
        encodedNameToJSFile += encodedName -> jsFile
      data
    }
    new Analyzer(logger, allData)
  }

  private def readData(infoFile: File): ClassInfoData = {
    implicit val formats = DefaultFormats
    val input = infoFile.input
    val reader = new java.io.InputStreamReader(infoFile.input)
    try {
      Extraction.extract[ClassInfoData](JsonParser.parse(reader, false))
    } finally {
      reader.close()
    }
  }

  private def writeDCEedOutput(analyzer: Analyzer, outputFile: File,
      coreJSLibFile: File, customScripts: Seq[File]): Unit = {

    val writer = new java.io.PrintWriter(outputFile.output)

    def pasteFile(f: File): Unit =
      pasteLines(fs.readLines(f))
    def pasteLines(lines: TraversableOnce[String]): Unit =
      lines foreach writer.println
    def pasteLine(line: String): Unit =
      writer.println(line)

    pasteFile(coreJSLibFile)

    def compareClassInfo(lhs: analyzer.ClassInfo, rhs: analyzer.ClassInfo) = {
      if (lhs.ancestorCount != rhs.ancestorCount) lhs.ancestorCount < rhs.ancestorCount
      else lhs.encodedName.compareTo(rhs.encodedName) < 0
    }

    for {
      classInfo <- analyzer.classInfos.values.toSeq.sortWith(compareClassInfo)
      if classInfo.isNeededAtAll
      jsFile <- encodedNameToJSFile.get(classInfo.encodedName)
    } {
      def pasteReachableMethods(methodLines: List[String], methodLinePrefix: String): Unit = {
        for (p <- methodChunks(methodLines, methodLinePrefix)) {
          val (optMethodName, methodLines) = p
          val isReachable = optMethodName.forall(
              classInfo.methodInfos(_).isReachable)
          if (isReachable)
            pasteLines(methodLines)
        }
      }

      val lines = fs.readLines(jsFile).filterNot(_.startsWith("//@"))
      if (classInfo.isImplClass) {
        pasteReachableMethods(lines, "ScalaJS.impls")
      } else if (!classInfo.hasInstantiation) {
        // there is only the data anyway
        pasteLines(lines)
      } else {
        val className = classInfo.encodedName
        val (implementation, afterImpl) = lines.span(!_.startsWith("ScalaJS.is."))
        val (classData, setClassData :: moduleAccessor) = afterImpl.span(!_.startsWith("ScalaJS.c."))

        if (classInfo.isAnySubclassInstantiated) {
          // constructor
          val (constructorLines0, constructorLine1 :: afterConstructor) =
            implementation.span(!_.startsWith(s"ScalaJS.c.$className.prototype.constructor ="))
          pasteLines(constructorLines0)
          pasteLine(constructorLine1)

          // methods
          val (methodLines, afterMethods) = afterConstructor.span(_ != "/** @constructor */")
          pasteReachableMethods(methodLines, s"ScalaJS.c.$className.prototype")

          // inheritable constructor
          pasteLines(afterMethods)
        }

        if (classInfo.isDataAccessed)
          pasteLines(classData)
        if (classInfo.isAnySubclassInstantiated)
          pasteLines(setClassData :: Nil)
        if (classInfo.isModuleAccessed)
          pasteLines(moduleAccessor)
      }
    }

    for (file <- customScripts)
      pasteFile(file)

    writer.close()
  }

  private def methodChunks(methodLines: List[String],
      methodLinePrefix: String): JustAForeach[(Option[String], List[String])] = {
    new JustAForeach[(Option[String], List[String])] {
      private[this] val prefixLength = methodLinePrefix.length

      override def foreach[U](f: ((Option[String], List[String])) => U): Unit = {
        @tailrec
        def loop(remainingLines: List[String]): Unit = {
          if (remainingLines.nonEmpty) {
            val firstLine = remainingLines.head
            val (methodLines, nextLines) = remainingLines.tail.span(!_.startsWith(methodLinePrefix))
            val encodedName = if (firstLine(prefixLength) == '.') {
              val name = firstLine.substring(prefixLength+1).takeWhile(_ != ' ')
              val unquoted = parse('"'+name+'"').asInstanceOf[JString].s // see #330
              Some(unquoted)
            } else {
              None // this is an exported method with []-select
            }
            f((encodedName, firstLine :: methodLines))
            loop(nextLines)
          }
        }
        loop(methodLines)
      }
    }
  }
}

object ScalaJSOptimizer {
  private trait JustAForeach[A] {
    def foreach[U](f: A => U): Unit
  }
}
