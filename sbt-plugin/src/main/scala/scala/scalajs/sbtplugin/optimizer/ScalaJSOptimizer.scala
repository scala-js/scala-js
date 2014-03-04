package scala.scalajs.sbtplugin.optimizer

import scala.annotation.tailrec

import scala.collection.mutable

import sbt._
import net.liftweb.json._

import scala.scalajs.sbtplugin.Utils._
import OptData._

/** Scala.js optimizer: does type-aware global dce. */
class ScalaJSOptimizer(logger: Logger) {
  val encodedNameToJSFile = mutable.Map.empty[String, File]

  def optimize(inputs: Seq[File], output: File,
      relativeSourceMaps: Boolean): Unit = {

    val (classFiles, otherFiles) = inputs.partition(isScalaJSClassFile)
    val infoFiles0 = classFiles.map(changeExt(_, ".js", ".sjsinfo"))

    val coreJSLibFile +: customScripts = otherFiles
    val coreJSLibDir = coreJSLibFile.getParentFile
    val infoFiles = Seq(
        coreJSLibDir / "javalangObject.sjsinfo",
        coreJSLibDir / "javalangString.sjsinfo") ++ infoFiles0

    val analyzer = parseInfoFiles(infoFiles)
    analyzer.computeReachability()
    writeDCEedOutput(analyzer, output, coreJSLibFile, customScripts)
  }

  def parseInfoFiles(infoFiles: Seq[File]): Analyzer = {
    val allData = infoFiles map { infoFile =>
      val data = readData(infoFile)
      val encodedName = data.encodedName
      val jsFile = changeExt(infoFile, ".sjsinfo", ".js")
      if (jsFile.exists)
        encodedNameToJSFile += encodedName -> jsFile
      data
    }
    new Analyzer(logger, allData)
  }

  def writeDCEedOutput(analyzer: Analyzer, output: File, coreJSLibFile: File,
      customScripts: Seq[File]): Unit = {

    val writer = new java.io.PrintWriter(output)

    def pasteFile(f: File): Unit =
      pasteLines(IO.readLines(f))
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

      val lines = IO.readLines(jsFile).filterNot(_.startsWith("//@"))
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

  trait JustAForeach[A] {
    def foreach[U](f: A => U): Unit
  }

  def methodChunks(methodLines: List[String],
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
