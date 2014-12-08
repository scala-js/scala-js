/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.optimizer

import scala.annotation.tailrec

import scala.collection.mutable

import org.scalajs.core.ir
import ir.{ClassKind, Definitions, Infos}

import org.scalajs.core.tools.sem._
import org.scalajs.core.tools.javascript.LongImpl
import org.scalajs.core.tools.logging._

import ScalaJSOptimizer._

class Analyzer(logger0: Logger, semantics: Semantics,
    allData: Seq[Infos.ClassInfo], globalWarnEnabled: Boolean,
    isBeforeOptimizer: Boolean) {
  import Analyzer._

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
    import semantics._
    import CheckedBehavior._

    implicit val from = FromCore

    def instantiateClassWith(className: String, constructor: String): ClassInfo = {
      val info = lookupClass(className)
      info.instantiated()
      info.callMethod(constructor)
      info
    }

    val ObjectClass = instantiateClassWith("O", "init___")
    ObjectClass.callMethod("toString__T")
    ObjectClass.callMethod("equals__O__Z")

    instantiateClassWith("jl_NullPointerException", "init___")

    if (asInstanceOfs != Unchecked)
      instantiateClassWith("jl_ClassCastException", "init___T")

    if (asInstanceOfs == Fatal)
      instantiateClassWith("sjsr_UndefinedBehaviorError", "init___jl_Throwable")

    instantiateClassWith("jl_Class", "init___jl_ScalaJSClassData")

    val RTStringModuleClass = lookupClass("sjsr_RuntimeString$")
    RTStringModuleClass.accessModule()
    RTStringModuleClass.callMethod("hashCode__T__I")

    val RTLongClass = lookupClass(LongImpl.RuntimeLongClass)
    RTLongClass.instantiated()
    for (method <- LongImpl.AllConstructors ++ LongImpl.AllMethods)
      RTLongClass.callMethod(method)

    if (isBeforeOptimizer) {
      for (method <- LongImpl.AllIntrinsicMethods)
        RTLongClass.callMethod(method)
    }

    val RTLongModuleClass = lookupClass(LongImpl.RuntimeLongModuleClass)
    RTLongModuleClass.accessModule()
    for (method <- LongImpl.AllModuleMethods)
      RTLongModuleClass.callMethod(method)

    if (isBeforeOptimizer) {
      for (hijacked <- Definitions.HijackedClasses)
        lookupClass(hijacked).instantiated()
    } else {
      for (hijacked <- Definitions.HijackedClasses)
        lookupClass(hijacked).accessData()
    }

    if (semantics.strictFloats) {
      val RuntimePackage = lookupClass("sjsr_package$")
      RuntimePackage.accessModule()
      RuntimePackage.callMethod("froundPolyfill__D__D")
    }

    val BitsModuleClass = lookupClass("sjsr_Bits$")
    BitsModuleClass.accessModule()
    BitsModuleClass.callMethod("numberHashCode__D__I")
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
    private[this] var _linking = false
    private[this] var _linked = false

    val encodedName = data.encodedName
    val isStaticModule = data.kind == ClassKind.ModuleClass
    val isInterface = data.kind == ClassKind.Interface
    val isRawJSType = data.kind == ClassKind.RawJSType
    val isHijackedClass = data.kind == ClassKind.HijackedClass
    val isClass = !isInterface && !isRawJSType
    val isExported = data.isExported

    var superClass: ClassInfo = _
    var ancestors: List[ClassInfo] = _
    val descendants = mutable.ListBuffer.empty[ClassInfo]

    var nonExistent: Boolean = false
    var warnEnabled: Boolean = true

    /** Ensures that this class and its dependencies are linked.
     *
     *  @throws CyclicDependencyException if this class is already linking
     */
    def linkClasses(): Unit = {
      if (_linking)
        throw CyclicDependencyException(encodedName :: Nil)

      if (!_linked) {
        _linking = true
        try {
          linkClassesImpl()
        } catch {
          case CyclicDependencyException(chain) =>
            throw CyclicDependencyException(encodedName :: chain)
        }
        _linking = false
        _linked = true
      }
    }

    private[this] def linkClassesImpl(): Unit = {
      if (data.superClass != "")
        superClass = lookupClass(data.superClass)

      ancestors = this +: data.parents.flatMap { anc =>
        val cls = lookupClass(anc)
        cls.linkClasses()
        cls.ancestors
      }.distinct

      for (ancestor <- ancestors)
        ancestor.descendants += this
    }

    lazy val ancestorCount: Int =
      if (superClass == null) 0
      else superClass.ancestorCount + 1

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
      isAnyStaticMethodReachable

    def isAnyStaticMethodReachable =
      staticMethodInfos.values.exists(_.isReachable)

    lazy val (methodInfos, staticMethodInfos) = {
      val allInfos = for (methodData <- data.methods)
        yield (methodData.encodedName, new MethodInfo(this, methodData))
      val (staticMethodInfos, methodInfos) = allInfos.partition(_._2.isStatic)
      (mutable.Map(methodInfos: _*), mutable.Map(staticMethodInfos: _*))
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
      assert(isClass,
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

    def lookupStaticMethod(methodName: String): MethodInfo = {
      tryLookupStaticMethod(methodName).getOrElse {
        val syntheticData = createMissingMethodInfo(methodName, isStatic = true)
        val m = new MethodInfo(this, syntheticData)
        m.nonExistent = true
        staticMethodInfos += methodName -> m
        m
      }
    }

    def tryLookupStaticMethod(methodName: String): Option[MethodInfo] =
      staticMethodInfos.get(methodName)

    override def toString(): String = encodedName

    /** Start reachability algorithm with the exports for that class. */
    def reachExports(): Unit = {
      implicit val from = FromExports

      // Myself
      if (isExported) {
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
      if (!isInstantiated && isClass) {
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
      if (!isAnySubclassInstantiated && isClass) {
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
      if (!isDataAccessed) {
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

    def callMethod(methodName: String, statically: Boolean = false)(
        implicit from: From): Unit = {
      logger.debugIndent(s"calling${if (statically) " statically" else ""}: $this.$methodName") {
        if (isConstructorName(methodName)) {
          // constructors are always implicitly called statically
          lookupMethod(methodName).reachStatic()
        } else if (statically) {
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

    def callStaticMethod(methodName: String)(implicit from: From): Unit = {
      logger.debugIndent(s"calling static method $this.$methodName") {
        lookupStaticMethod(methodName).reachStatic()
      }
    }
  }

  class MethodInfo(val owner: ClassInfo, data: Infos.MethodInfo) {

    val encodedName = data.encodedName
    val isStatic = data.isStatic
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
      assert(!isStatic,
          s"Trying to dynamically reach the static method $this")
      assert(owner.isClass,
          s"Trying to dynamically reach the non-class method $this")
      assert(!isConstructorName(encodedName),
          s"Trying to dynamically reach the constructor $this")

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

        for (moduleName <- data.accessedModules) {
          lookupModule(moduleName).accessModule()
        }

        for (className <- data.instantiatedClasses) {
          lookupClass(className).instantiated()
        }

        for (className <- data.accessedClassData) {
          lookupClass(className).accessData()
        }

        for ((className, methods) <- data.methodsCalled) {
          val classInfo = lookupClass(className)
          for (methodName <- methods)
            classInfo.callMethod(methodName)
        }

        for ((className, methods) <- data.methodsCalledStatically) {
          val classInfo = lookupClass(className)
          for (methodName <- methods)
            classInfo.callMethod(methodName, statically = true)
        }

        for ((className, methods) <- data.staticMethodsCalled) {
          val classInfo = lookupClass(className)
          for (methodName <- methods)
            classInfo.callStaticMethod(methodName)
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
      if (encodedName.endsWith("$")) ClassKind.ModuleClass // wild guess
      else ClassKind.Class
    Infos.ClassInfo(
        name = s"<$encodedName>",
        encodedName = encodedName,
        isExported = false,
        kind = kind,
        superClass = if (kind.isClass) "O" else "",
        parents = List("O"),
        methods = List(
            createMissingMethodInfo("__init__"),
            createMissingMethodInfo("init___"))
    )
  }

  private def createMissingMethodInfo(encodedName: String,
      isStatic: Boolean = false,
      isAbstract: Boolean = false): Infos.MethodInfo = {
    Infos.MethodInfo(encodedName = encodedName,
        isStatic = isStatic, isAbstract = isAbstract)
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

object Analyzer {
  final case class CyclicDependencyException(
      chain: List[String]) extends Exception(mkMsg(chain))

  private def mkMsg(chain: List[String]) = {
    val buf = new StringBuffer
    buf.append("A cyclic dependency has been encountered: \n")
    for (elem <- chain)
      buf.append(s"  - $elem\n")
    buf.toString
  }
}
