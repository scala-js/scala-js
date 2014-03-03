package scala.scalajs.sbtplugin.optimizer

import scala.annotation.tailrec

import scala.collection.mutable

import sbt.Logger
import sbt.Level.{Value => LogLevel}

import OptData._

class Analyzer(logger0: Logger, allData: Seq[ClassInfoData]) {
  object logger extends Logger {
    var indentation: String = ""

    def indent(): Unit = indentation += "  "
    def undent(): Unit = indentation = indentation.substring(2)

    def log(level: LogLevel, message: => String) =
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
      debug(message)
      indented(body)
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

  def computeReachability(): Unit = {
    reachCoreSymbols()
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

    lookupClass("scala_runtime_BoxedUnit$").accessModule()
    lookupClass("java_lang_Boolean$").accessModule()
    for ((name, char) <- Seq(
        ("Character", "C"), ("Byte", "B"), ("Short", "S"), ("Integer", "I"),
        ("Long", "J"), ("Float", "F"), ("Double", "D"))) {
      instantiateClassWith(s"java_lang_$name", s"init___$char")
    }

    instantiateClassWith("java_lang_ClassCastException", "init___T")
    instantiateClassWith("scala_scalajs_js_JavaScriptException", "init___Lscala_scalajs_js_Any")

    instantiateClassWith("java_lang_Class", "init___Lscala_scalajs_js_Dynamic")

    val LongModule = lookupClass("scala_scalajs_runtime_Long$")
    LongModule.accessModule()
    LongModule.callMethod("zero__Lscala_scalajs_runtime_Long")

    val BoxesRunTime = lookupClass("scala_runtime_BoxesRunTime$")
    BoxesRunTime.accessModule()
    BoxesRunTime.callMethod("equals__O__O__Z")
  }

  class ClassInfo(data: ClassInfoData) {
    val encodedName = data.encodedName
    val ancestorCount = data.ancestorCount
    val isStaticModule = data.isStaticModule
    val isInterface = data.isInterface
    val isImplClass = data.isImplClass
    val isRawJSType = data.isRawJSType
    val isClass = !isInterface && !isImplClass && !isRawJSType
    val isExported = data.isExported.getOrElse(false)

    val hasInstantiation = isClass
    val hasData = !isImplClass

    var superClass: ClassInfo = _
    val ancestors = mutable.ListBuffer.empty[ClassInfo]
    val descendants = mutable.ListBuffer.empty[ClassInfo]

    var nonExistent: Boolean = false

    def linkClasses(): Unit = {
      if (data.superClass != "")
        superClass = lookupClass(data.superClass)
      ancestors ++= data.ancestors.map(lookupClass)
      for (ancestor <- ancestors)
        ancestor.descendants += this
    }

    lazy val descendentClasses = descendants.filter(_.isClass)

    var isInstantiated: Boolean = false
    var isModuleAccessed: Boolean = false
    var isDataAccessed: Boolean = false

    var instantiatedFrom: Option[From] = None

    def isNeededAtAll =
      isDataAccessed ||
      (isImplClass && methodInfos.values.exists(_.isReachable))

    lazy val methodInfos: mutable.Map[String, MethodInfo] = {
      val ms = for ((methodName, methodData) <- data.methods)
        yield (methodName, new MethodInfo(this, methodName, methodData))
      mutable.Map.empty[String, MethodInfo] ++ ms
    }

    def lookupMethod(methodName: String)(implicit from: From): MethodInfo = {
      tryLookupMethod(methodName).getOrElse {
        val existsInAnInterface =
          ancestors.exists { info =>
            info.isInterface && info.methodInfos.contains(methodName)
          }
        val syntheticData = {
          if (existsInAnInterface)
            MethodInfoData.placeholder(methodName, isAbstract = true)
          else {
            logger.temporarilyNotIndented {
              logger.warn(s"Referring to non-existent method $this.$methodName")
              warnCallStack()
            }
            MethodInfoData.placeholder(methodName)
          }
        }
        val m = new MethodInfo(this, methodName, syntheticData)
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
            case Some(m) => Some(m)
            case None => loop(ancestorInfo.superClass)
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
        assert(!isImplClass, "An implementation must not be exported")
        if (isStaticModule) accessModule()
        else instantiated()
      }

      // My methods
      for (methodInfo <- methodInfos.values) {
        if (methodInfo.isExported)
          methodInfo.reach()
      }
    }

    def accessModule()(implicit from: From): Unit = {
      assert(isStaticModule, s"Cannot call accessModule() on non-module $this")
      if (!isModuleAccessed) {
        logger.debugIndent(s"$this.isModuleAccessed = true") {
          isModuleAccessed = true
          instantiated()
          methodInfos("init___").reach()
        }
      }
    }

    def instantiated()(implicit from: From): Unit = {
      if (!isInstantiated && hasInstantiation) {
        logger.debugIndent(s"$this.isInstantiated = true") {
          isInstantiated = true
          instantiatedFrom = Some(from)
          ancestors.foreach(_.instantiated())
          accessData()
          for (methodInfo <- methodInfos.values; if methodInfo.isReachable)
            methodInfo.doReach()
          methodInfos.get("__init__").foreach(_.reach())
        }
      }
    }

    def accessData()(implicit from: From): Unit = {
      if (!isDataAccessed && hasData) {
        checkExistent()
        logger.debug(s"$this.isDataAccessed = true")
        isDataAccessed = true
      }
    }

    def checkExistent(): Unit = {
      if (nonExistent) {
        logger.warn(s"Referring to non-existent class $encodedName")
        nonExistent = false
      }
    }

    def callMethod(methodName: String)(implicit from: From): Unit = {
      logger.debugIndent(s"calling $this.$methodName") {
        if (isImplClass) {
          // make sure it exists in this class
          lookupMethod(methodName).reach()
        } else if (methodName == "__init__" || methodName.startsWith("init__")) {
          // constructors are not inherited
          lookupMethod(methodName).reach()
        } else {
          for (descendentClass <- descendentClasses) {
            if (isReflProxyName(methodName)) {
              descendentClass.tryLookupMethod(methodName).foreach(_.reach())
            } else {
              descendentClass.lookupMethod(methodName).reach()
            }
          }
        }
      }
    }
  }

  class MethodInfo(val owner: ClassInfo, val encodedName: String,
      data: MethodInfoData) {

    val isAbstract = data.isAbstract.getOrElse(false)
    val isExported = data.isExported.getOrElse(false)
    val isReflProxy = isReflProxyName(encodedName)

    var isReachable: Boolean = false // provided owner is instantiated

    var calledFrom: Option[From] = None

    override def toString(): String = s"$owner.$encodedName"

    def reach()(implicit from: From): Unit = {
      if (!isReachable && !isAbstract) {
        logger.debugIndent(s"$this.isReachable = true") {
          isReachable = true
          calledFrom = Some(from)
          if (owner.isInstantiated || owner.isImplClass)
            doReach()
        }
      }
    }

    def doReach(): Unit = {
      assert(isReachable && (owner.isInstantiated || owner.isImplClass))
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
      }
    }
  }

  def isReflProxyName(encodedName: String): Boolean = {
    encodedName.endsWith("__") &&
    (encodedName != "init___") && (encodedName != "__init__")
  }

  def warnCallStack()(implicit from: From): Unit = {
    val seenInfos = mutable.Set.empty[AnyRef]

    def onlyOnce(info: AnyRef): Boolean = {
      if (seenInfos.add(info)) {
        true
      } else {
        logger.warn("  (already seen, not repeating call stack)")
        false
      }
    }

    def rec(optFrom: Option[From], verb: String = "called"): Unit = {
      val involvedClasses = new mutable.ListBuffer[ClassInfo]

      @tailrec
      def loopTrace(optFrom: Option[From], verb: String = "called"): Unit = {
        optFrom match {
          case None =>
            logger.warn(s"$verb from ... er ... nowhere!? (this is a bug in dce)")
          case Some(from) =>
            from match {
              case FromMethod(methodInfo) =>
                logger.warn(s"$verb from $methodInfo")
                if (onlyOnce(methodInfo)) {
                  val classInfo = methodInfo.owner
                  if (classInfo.hasInstantiation)
                    involvedClasses += classInfo
                  loopTrace(methodInfo.calledFrom)
                }
              case FromCore =>
                logger.warn(s"$verb from scalajs-corejslib.js")
              case FromExports =>
                logger.warn("exported to JavaScript with @JSExport")
            }
        }
      }

      logger.indented {
        loopTrace(optFrom, verb = verb)
      }

      if (involvedClasses.nonEmpty) {
        logger.warn("involving instantiated classes:")
        logger.indented {
          for (classInfo <- involvedClasses.result().distinct) {
            logger.warn(s"$classInfo")
            if (onlyOnce(classInfo))
              rec(classInfo.instantiatedFrom, verb = "instantiated")
          }
        }
      }
    }

    rec(Some(from))
  }
}
