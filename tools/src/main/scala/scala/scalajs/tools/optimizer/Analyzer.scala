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

import scala.scalajs.ir
import ir.{ClassKind, Infos}

import scala.scalajs.tools.logging._

import ScalaJSOptimizer._

class Analyzer(logger0: Logger, allData: Seq[Infos.ClassInfo],
    globalWarnEnabled: Boolean = true) {
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

  private val HijackedClassNames = Set(
      "sr_BoxedUnit", "jl_Boolean", "jl_Byte", "jl_Short", "jl_Integer",
      "jl_Long", "jl_Float", "jl_Double", "T"
  )

  var allAvailable: Boolean = true

  val classInfos: mutable.Map[String, ClassInfo] = {
    val cs = for (classData <- allData)
      yield (classData.encodedName, new ClassInfo(classData))
    mutable.Map.empty[String, ClassInfo] ++ cs
  }

  def lookupClass(encodedName: String): ClassInfo = {
    classInfos.get(encodedName) match {
      case Some(info) => info
      case None =>
        val c = new ClassInfo(createMissingClassInfo(encodedName))
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
    if (!classInfos.contains(ir.Definitions.ObjectClass))
      sys.error("Fatal error: could not find java.lang.Object on the classpath")
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

    val ObjectClass = instantiateClassWith("O", "init___")
    ObjectClass.callMethod("toString__T")

    instantiateClassWith(s"jl_Character", s"init___C")

    instantiateClassWith("jl_ClassCastException", "init___T")
    instantiateClassWith("sjs_js_JavaScriptException", "init___sjs_js_Any")

    instantiateClassWith("jl_Class", "init___jl_ScalaJSClassData")

    val LongModule = lookupClass("sjsr_RuntimeLong$")
    LongModule.accessModule()
    LongModule.callMethod("zero__sjsr_RuntimeLong")
    LongModule.callMethod("fromDouble__D__sjsr_RuntimeLong")

    val BoxesRunTime = lookupClass("sr_BoxesRunTime$")
    BoxesRunTime.accessModule()
    BoxesRunTime.callMethod("equals__O__O__Z")

    for (hijacked <- HijackedClassNames)
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

  class ClassInfo(data: Infos.ClassInfo) {
    val encodedName = data.encodedName
    val ancestorCount = data.ancestorCount
    val isStaticModule = data.kind == ClassKind.ModuleClass
    val isInterface = data.kind == ClassKind.Interface
    val isImplClass = data.kind == ClassKind.TraitImpl
    val isRawJSType = data.kind == ClassKind.RawJSType
    val isHijackedClass = HijackedClassNames.contains(encodedName)
    val isClass = !isInterface && !isImplClass && !isRawJSType
    val isExported = data.isExported

    val hasInstantiation = isClass && !isHijackedClass
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

    def optimizerHints: Infos.OptimizerHints = data.optimizerHints

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
      val ms = for (methodData <- data.methods)
        yield (methodData.encodedName, new MethodInfo(this, methodData))
      mutable.Map.empty[String, MethodInfo] ++ ms
    }

    def lookupMethod(methodName: String): MethodInfo = {
      tryLookupMethod(methodName).getOrElse {
        val syntheticData = createMissingMethodInfo(methodName)
        val m = new MethodInfo(this, syntheticData)
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

    def checkExistent()(implicit from: From): Unit = {
      if (nonExistent) {
        if (warnEnabled && globalWarnEnabled) {
          logger.warn(s"Referring to non-existent class $encodedName")
          warnCallStack()
        }
        nonExistent = false
        allAvailable = false
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

  class MethodInfo(val owner: ClassInfo, data: Infos.MethodInfo) {

    val encodedName = data.encodedName
    val isAbstract = data.isAbstract
    val isExported = data.isExported
    val isReflProxy = isReflProxyName(encodedName)

    def optimizerHints: Infos.OptimizerHints = data.optimizerHints

    var isReachable: Boolean = false

    var calledFrom: Option[From] = None
    var instantiatedSubclass: Option[ClassInfo] = None

    var nonExistent: Boolean = false
    var warnEnabled: Boolean = true

    override def toString(): String = s"$owner.$encodedName"

    def reachStatic()(implicit from: From): Unit = {
      assert(!isAbstract,
          s"Trying to reach statically the abstract method $this")

      checkExistent()

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

      checkExistent()

      if (!isReachable) {
        logger.debugIndent(s"$this.isReachable = true") {
          isReachable = true
          calledFrom = Some(from)
          instantiatedSubclass = Some(inClass)
          doReach()
        }
      }
    }

    private def checkExistent()(implicit from: From) = {
      if (nonExistent) {
        if (warnEnabled && owner.warnEnabled && globalWarnEnabled) {
          logger.temporarilyNotIndented {
            logger.warn(s"Referring to non-existent method $this")
            warnCallStack()
          }
        }
        allAvailable = false
      }
    }

    private[this] def doReach(): Unit = {
      logger.debugIndent(s"$this.doReach()") {
        implicit val from = FromMethod(this)

        if (owner.isImplClass)
          owner.checkExistent()

        for (moduleName <- data.accessedModules) {
          lookupModule(moduleName).accessModule()
        }

        for (className <- data.instantiatedClasses) {
          lookupClass(className).instantiated()
        }

        for (className <- data.accessedClassData) {
          lookupClass(className).accessData()
        }

        for ((className, methods) <- data.calledMethods) {
          val classInfo = lookupClass(className)
          for (methodName <- methods)
            classInfo.callMethod(methodName)
        }

        for ((className, methods) <- data.calledMethodsStatic) {
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

  private def createMissingClassInfo(encodedName: String): Infos.ClassInfo = {
    val kind =
      if (encodedName.endsWith("$")) ClassKind.ModuleClass
      else if (encodedName.endsWith("$class")) ClassKind.TraitImpl
      else ClassKind.Class
    Infos.ClassInfo(
        name = s"<$encodedName>",
        encodedName = encodedName,
        isExported = false,
        ancestorCount = if (kind.isClass) 1 else 0,
        kind = kind,
        superClass = if (kind.isClass) "O" else "",
        ancestors = List(encodedName, "O"),
        methods = List(
            createMissingMethodInfo("__init__"),
            createMissingMethodInfo("init___"))
    )
  }

  private def createMissingMethodInfo(encodedName: String,
      isAbstract: Boolean = false): Infos.MethodInfo = {
    Infos.MethodInfo(encodedName = encodedName, isAbstract = isAbstract)
  }

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
