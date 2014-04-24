/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.tools.optimizer

import scala.annotation.tailrec

import scala.collection.mutable

import scala.scalajs.tools.logging._

import OptData._
import ScalaJSOptimizer._

class Analyzer(logger0: Logger, allData: Seq[ClassInfoData]) {
  /* Set this to true to debug the DCE analyzer.
   * We don't rely on config to disable 'debug' messages because we want
   * to use 'debug' for displaying more stack trace info that the user can
   * see with the 'last' command.
   */
  val DebugAnalyzer = false

  object logger extends Logger {
    var indentation: String = ""

    def indent(): Unit = indentation += "  "
    def undent(): Unit = indentation = indentation.substring(2)

    def log(level: Level, message: => String) =
      logger0.log(level, indentation+message)
    def success(message: => String) =
      logger0.success(indentation+message)
    def trace(t: => Throwable) =
      logger0.trace(t)

    def indented[A](body: => A): A = {
      indent()
      try body
      finally undent()
    }

    def debugIndent[A](message: => String)(body: => A): A = {
      if (DebugAnalyzer) {
        debug(message)
        indented(body)
      } else {
        body
      }
    }

    def temporarilyNotIndented[A](body: => A): A = {
      val savedIndent = indentation
      indentation = ""
      try body
      finally indentation = savedIndent
    }
  }

  sealed trait From
  case class FromMethod(methodInfo: MethodInfo) extends From
  case object FromCore extends From
  case object FromExports extends From
  case object FromManual extends From

  private val HijackedBoxedClassNames = Set(
      "scala_runtime_BoxedUnit", "java_lang_Boolean",
      "java_lang_Byte", "java_lang_Short", "java_lang_Integer",
      "java_lang_Long", "java_lang_Float", "java_lang_Double"
  )

  val classInfos: mutable.Map[String, ClassInfo] = {
    val cs = for (classData <- allData)
      yield (classData.encodedName, new ClassInfo(classData))
    mutable.Map.empty[String, ClassInfo] ++ cs
  }

  def lookupClass(encodedName: String): ClassInfo = {
    classInfos.get(encodedName) match {
      case Some(info) => info
      case None =>
        val c = new ClassInfo(ClassInfoData.placeholder(encodedName))
        classInfos += encodedName -> c
        c.nonExistent = true
        c.linkClasses()
        c
    }
  }

  def lookupModule(encodedName: String): ClassInfo = {
    lookupClass(encodedName+"$")
  }

  linkClasses()

  def linkClasses(): Unit = {
    for (classInfo <- classInfos.values.toList)
      classInfo.linkClasses()
  }

  def computeReachability(manuallyReachable: Seq[ManualReachability],
      noWarnMissing: Seq[NoWarnMissing]): Unit = {
    // Stuff reachable from core symbols always should warn
    reachCoreSymbols()

    // Disable warnings as requested
    noWarnMissing.foreach(disableWarning _)

    // Reach all user stuff
    manuallyReachable.foreach(reachManually _)
    for (classInfo <- classInfos.values)
      classInfo.reachExports()
  }

  /** Reach symbols used directly by scalajsenv.js. */
  def reachCoreSymbols(): Unit = {
    implicit val from = FromCore

    def instantiateClassWith(className: String, constructor: String): ClassInfo = {
      val info = lookupClass(className)
      info.instantiated()
      info.callMethod(constructor)
      info
    }

    val ObjectClass = instantiateClassWith("java_lang_Object", "init___")
    ObjectClass.callMethod("toString__T")

    instantiateClassWith(s"java_lang_Character", s"init___C")

    instantiateClassWith("java_lang_ClassCastException", "init___T")
    instantiateClassWith("scala_scalajs_js_JavaScriptException", "init___Lscala_scalajs_js_Any")

    instantiateClassWith("java_lang_Class", "init___Lscala_scalajs_js_Dynamic")

    val LongModule = lookupClass("scala_scalajs_runtime_RuntimeLong$")
    LongModule.accessModule()
    LongModule.callMethod("zero__Lscala_scalajs_runtime_RuntimeLong")
    LongModule.callMethod("fromDouble__D__Lscala_scalajs_runtime_RuntimeLong")

    val BoxesRunTime = lookupClass("scala_runtime_BoxesRunTime$")
    BoxesRunTime.accessModule()
    BoxesRunTime.callMethod("equals__O__O__Z")

    for (hijacked <- HijackedBoxedClassNames)
      lookupClass(hijacked).accessData()
  }

  def reachManually(info: ManualReachability) = {
    implicit val from = FromManual

    // Don't lookupClass here, since we don't want to create any
    // symbols. If a symbol doesn't exist, we fail.
    info match {
      case ReachObject(name) => classInfos(name + "$").accessModule()
      case Instantiate(name) => classInfos(name).instantiated()
      case ReachMethod(className, methodName, static) =>
        classInfos(className).callMethod(methodName, static)
    }
  }

  def disableWarning(noWarn: NoWarnMissing) = noWarn match {
    case NoWarnClass(className) =>
      lookupClass(className).warnEnabled = false
    case NoWarnMethod(className, methodName) =>
      lookupClass(className).lookupMethod(methodName).warnEnabled = false
  }

  class ClassInfo(data: ClassInfoData) {
    val encodedName = data.encodedName
    val ancestorCount = data.ancestorCount
    val isStaticModule = data.isStaticModule
    val isInterface = data.isInterface
    val isImplClass = data.isImplClass
    val isRawJSType = data.isRawJSType
    val isHijackedBoxedClass = HijackedBoxedClassNames.contains(encodedName)
    val isClass = !isInterface && !isImplClass && !isRawJSType
    val isExported = data.isExported.getOrElse(false)

    val hasInstantiation = isClass && !isHijackedBoxedClass
    val hasData = !isImplClass

    var superClass: ClassInfo = _
    val ancestors = mutable.ListBuffer.empty[ClassInfo]
    val descendants = mutable.ListBuffer.empty[ClassInfo]

    var nonExistent: Boolean = false
    var warnEnabled: Boolean = true

    def linkClasses(): Unit = {
      if (data.superClass != "")
        superClass = lookupClass(data.superClass)
      ancestors ++= data.ancestors.map(lookupClass)
      for (ancestor <- ancestors)
        ancestor.descendants += this
    }

    lazy val descendentClasses = descendants.filter(_.isClass)

    var isInstantiated: Boolean = false
    var isAnySubclassInstantiated: Boolean = false
    var isModuleAccessed: Boolean = false
    var isDataAccessed: Boolean = false

    var instantiatedFrom: Option[From] = None

    val delayedCalls = mutable.Map.empty[String, From]

    def isNeededAtAll =
      isDataAccessed ||
      isAnySubclassInstantiated ||
      (isImplClass && methodInfos.values.exists(_.isReachable))

    lazy val methodInfos: mutable.Map[String, MethodInfo] = {
      val ms = for ((methodName, methodData) <- data.methods)
        yield (methodName, new MethodInfo(this, methodName, methodData))
      mutable.Map.empty[String, MethodInfo] ++ ms
    }

    def lookupMethod(methodName: String): MethodInfo = {
      tryLookupMethod(methodName).getOrElse {
        val syntheticData = MethodInfoData.placeholder(methodName)
        val m = new MethodInfo(this, methodName, syntheticData)
        m.nonExistent = true
        methodInfos += methodName -> m
        m
      }
    }

    def tryLookupMethod(methodName: String): Option[MethodInfo] = {
      assert(isClass || isImplClass,
          s"Cannot call lookupMethod($methodName) on non-class $this")
      @tailrec
      def loop(ancestorInfo: ClassInfo): Option[MethodInfo] = {
        if (ancestorInfo ne null) {
          ancestorInfo.methodInfos.get(methodName) match {
            case Some(m) if !m.isAbstract => Some(m)
            case _ => loop(ancestorInfo.superClass)
          }
        } else {
          None
        }
      }
      loop(this)
    }

    override def toString(): String = encodedName

    /** Start reachability algorithm with the exports for that class. */
    def reachExports(): Unit = {
      implicit val from = FromExports

      // Myself
      if (isExported) {
        assert(!isImplClass, "An implementation class must not be exported")
        if (isStaticModule) accessModule()
        else instantiated()
      }

      // My methods
      for (methodInfo <- methodInfos.values) {
        if (methodInfo.isExported)
          callMethod(methodInfo.encodedName)
      }
    }

    def accessModule()(implicit from: From): Unit = {
      assert(isStaticModule, s"Cannot call accessModule() on non-module $this")
      if (!isModuleAccessed) {
        logger.debugIndent(s"$this.isModuleAccessed = true") {
          isModuleAccessed = true
          instantiated()
          callMethod("init___")
        }
      }
    }

    def instantiated()(implicit from: From): Unit = {
      if (!isInstantiated && hasInstantiation) {
        logger.debugIndent(s"$this.isInstantiated = true") {
          isInstantiated = true
          instantiatedFrom = Some(from)
          ancestors.foreach(_.subclassInstantiated())
        }

        for ((methodName, from) <- delayedCalls)
          delayedCallMethod(methodName)(from)
      }
    }

    private def subclassInstantiated()(implicit from: From): Unit = {
      if (!isAnySubclassInstantiated && hasInstantiation) {
        logger.debugIndent(s"$this.isAnySubclassInstantiated = true") {
          isAnySubclassInstantiated = true
          if (instantiatedFrom.isEmpty)
            instantiatedFrom = Some(from)
          accessData()
          methodInfos.get("__init__").foreach(_.reachStatic())
        }
      }
    }

    def accessData()(implicit from: From): Unit = {
      if (!isDataAccessed && hasData) {
        checkExistent()
        if (DebugAnalyzer)
          logger.debug(s"$this.isDataAccessed = true")
        isDataAccessed = true
      }
    }

    def checkExistent(): Unit = {
      if (nonExistent && warnEnabled) {
        logger.warn(s"Referring to non-existent class $encodedName")
        nonExistent = false
      }
    }

    def callMethod(methodName: String, static: Boolean = false)(
        implicit from: From): Unit = {
      logger.debugIndent(s"calling${if (static) " static" else ""} $this.$methodName") {
        if (isImplClass) {
          // methods in impl classes are always implicitly called statically
          lookupMethod(methodName).reachStatic()
        } else if (isConstructorName(methodName)) {
          // constructors are always implicitly called statically
          lookupMethod(methodName).reachStatic()
        } else if (static) {
          assert(!isReflProxyName(methodName),
              s"Trying to call statically refl proxy $this.$methodName")
          lookupMethod(methodName).reachStatic()
        } else {
          for (descendentClass <- descendentClasses) {
            if (descendentClass.isInstantiated)
              descendentClass.delayedCallMethod(methodName)
            else
              descendentClass.delayedCalls += ((methodName, from))
          }
        }
      }
    }

    private def delayedCallMethod(methodName: String)(implicit from: From): Unit = {
      if (isReflProxyName(methodName)) {
        tryLookupMethod(methodName).foreach(_.reach(this))
      } else {
        lookupMethod(methodName).reach(this)
      }
    }
  }

  class MethodInfo(val owner: ClassInfo, val encodedName: String,
      data: MethodInfoData) {

    val isAbstract = data.isAbstract.getOrElse(false)
    val isExported = data.isExported.getOrElse(false)
    val isReflProxy = isReflProxyName(encodedName)

    var isReachable: Boolean = false

    var calledFrom: Option[From] = None
    var instantiatedSubclass: Option[ClassInfo] = None

    var nonExistent: Boolean = false
    var warnEnabled: Boolean = true

    override def toString(): String = s"$owner.$encodedName"

    def reachStatic()(implicit from: From): Unit = {
      assert(!isAbstract,
          s"Trying to reach statically the abstract method $this")

      warnIfNonExistent()

      if (!isReachable) {
        logger.debugIndent(s"$this.isReachable = true") {
          isReachable = true
          calledFrom = Some(from)
          doReach()
        }
      }
    }

    def reach(inClass: ClassInfo)(implicit from: From): Unit = {
      assert(owner.isClass,
          s"Trying to reach dynamically the non-class method $this")
      assert(!isConstructorName(encodedName),
          s"Trying to reach dynamically the constructor $this")

      warnIfNonExistent()

      if (!isReachable) {
        logger.debugIndent(s"$this.isReachable = true") {
          isReachable = true
          calledFrom = Some(from)
          instantiatedSubclass = Some(inClass)
          doReach()
        }
      }
    }

    private def warnIfNonExistent()(implicit from: From) = {
      if (warnEnabled && owner.warnEnabled && nonExistent) {
        logger.temporarilyNotIndented {
          logger.warn(s"Referring to non-existent method $this")
          warnCallStack()
        }
      }
    }

    private[this] def doReach(): Unit = {
      logger.debugIndent(s"$this.doReach()") {
        if (owner.isImplClass)
          owner.checkExistent()

        implicit val from = FromMethod(this)

        for (moduleName <- data.accessedModules.getOrElse(Nil)) {
          lookupModule(moduleName).accessModule()
        }

        for (className <- data.instantiatedClasses.getOrElse(Nil)) {
          lookupClass(className).instantiated()
        }

        for (className <- data.accessedClassData.getOrElse(Nil)) {
          lookupClass(className).accessData()
        }

        for ((className, methods) <- data.calledMethods.getOrElse(Map.empty)) {
          val classInfo = lookupClass(className)
          for (methodName <- methods)
            classInfo.callMethod(methodName)
        }

        for ((className, methods) <- data.calledMethodsStatic.getOrElse(Map.empty)) {
          val classInfo = lookupClass(className)
          for (methodName <- methods)
            classInfo.callMethod(methodName, static = true)
        }
      }
    }
  }

  def isReflProxyName(encodedName: String): Boolean = {
    encodedName.endsWith("__") &&
    (encodedName != "init___") && (encodedName != "__init__")
  }

  def isConstructorName(encodedName: String): Boolean =
    encodedName.startsWith("init___") || (encodedName == "__init__")

  def warnCallStack()(implicit from: From): Unit = {
    val seenInfos = mutable.Set.empty[AnyRef]

    def rec(level: Level, optFrom: Option[From],
        verb: String = "called"): Unit = {
      val involvedClasses = new mutable.ListBuffer[ClassInfo]

      def onlyOnce(info: AnyRef): Boolean = {
        if (seenInfos.add(info)) {
          true
        } else {
          logger.log(level, "  (already seen, not repeating call stack)")
          false
        }
      }

      @tailrec
      def loopTrace(optFrom: Option[From], verb: String = "called"): Unit = {
        optFrom match {
          case None =>
            logger.log(level, s"$verb from ... er ... nowhere!? (this is a bug in dce)")
          case Some(from) =>
            from match {
              case FromMethod(methodInfo) =>
                logger.log(level, s"$verb from $methodInfo")
                if (onlyOnce(methodInfo)) {
                  methodInfo.instantiatedSubclass.foreach(involvedClasses += _)
                  loopTrace(methodInfo.calledFrom)
                }
              case FromCore =>
                logger.log(level, s"$verb from scalajs-corejslib.js")
              case FromExports =>
                logger.log(level, "exported to JavaScript with @JSExport")
              case FromManual =>
                logger.log(level, "manually made reachable")
            }
        }
      }

      logger.indented {
        loopTrace(optFrom, verb = verb)
      }

      if (involvedClasses.nonEmpty) {
        logger.log(level, "involving instantiated classes:")
        logger.indented {
          for (classInfo <- involvedClasses.result().distinct) {
            logger.log(level, s"$classInfo")
            if (onlyOnce(classInfo))
              rec(Level.Debug, classInfo.instantiatedFrom, verb = "instantiated")
            // recurse with Debug log level not to overwhelm the user
          }
        }
      }
    }

    rec(Level.Warn, Some(from))
  }
}
