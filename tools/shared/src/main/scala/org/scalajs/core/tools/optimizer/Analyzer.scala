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
import Definitions._

import org.scalajs.core.tools.sem._
import org.scalajs.core.tools.javascript.{LongImpl, OutputMode}

import ScalaJSOptimizer._

final class Analyzer(semantics: Semantics, outputMode: OutputMode,
    reachOptimizerSymbols: Boolean,
    allowAddingSyntheticMethods: Boolean) extends Analysis {
  import Analyzer._
  import Analysis._

  @deprecated("Use the overload with an explicit `allowAddingSyntheticMethods` flag", "0.6.6")
  def this(semantics: Semantics, outputMode: OutputMode,
      reachOptimizerSymbols: Boolean) = {
    this(semantics, outputMode, reachOptimizerSymbols,
        allowAddingSyntheticMethods = true)
  }

  @deprecated("Use the overload with an explicit output mode", "0.6.3")
  def this(semantics: Semantics, reachOptimizerSymbols: Boolean) =
    this(semantics, OutputMode.ECMAScript51Isolated, reachOptimizerSymbols)

  private[this] var _allAvailable: Boolean = true
  private[this] val _classInfos = mutable.Map.empty[String, ClassInfo]
  private[this] val _errors = mutable.Buffer.empty[Error]

  def allAvailable: Boolean = _allAvailable
  def classInfos: scala.collection.Map[String, Analysis.ClassInfo] = _classInfos
  def errors: Seq[Error] = _errors

  private def lookupClass(encodedName: String): ClassInfo = {
    _classInfos.get(encodedName) match {
      case Some(info) => info
      case None =>
        val c = new ClassInfo(createMissingClassInfo(encodedName))
        _classInfos += encodedName -> c
        c.nonExistent = true
        c.linkClasses()
        c
    }
  }

  def computeReachability(allData: Seq[Infos.ClassInfo]): Analysis = {
    require(_classInfos.isEmpty, "Cannot run the same Analyzer multiple times")

    // Load data
    for (classData <- allData)
      _classInfos += classData.encodedName -> new ClassInfo(classData)

    linkClasses()

    reachCoreSymbols()

    // Reach all user stuff
    for (classInfo <- _classInfos.values)
      classInfo.reachExports()

    // Reach additional data, based on reflection methods used
    reachDataThroughReflection()

    this
  }

  private def linkClasses(): Unit = {
    if (!_classInfos.contains(ir.Definitions.ObjectClass))
      sys.error("Fatal error: could not find java.lang.Object on the classpath")
    for (classInfo <- _classInfos.values.toList)
      classInfo.linkClasses()
  }

  /** Reach symbols used directly by scalajsenv.js. */
  private def reachCoreSymbols(): Unit = {
    import semantics._
    import CheckedBehavior._

    implicit val from = FromCore

    def instantiateClassWith(className: String, constructor: String): ClassInfo = {
      val info = lookupClass(className)
      info.instantiated()
      info.callMethod(constructor, statically = true)
      info
    }

    val ObjectClass = instantiateClassWith("O", "init___")
    ObjectClass.accessData()
    ObjectClass.callMethod("toString__T")
    ObjectClass.callMethod("equals__O__Z")

    instantiateClassWith("jl_NullPointerException", "init___")
    instantiateClassWith("jl_CloneNotSupportedException", "init___")

    if (asInstanceOfs != Unchecked)
      instantiateClassWith("jl_ClassCastException", "init___T")

    if (asInstanceOfs == Fatal)
      instantiateClassWith("sjsr_UndefinedBehaviorError", "init___jl_Throwable")

    if (moduleInit == Fatal)
      instantiateClassWith("sjsr_UndefinedBehaviorError", "init___T")

    instantiateClassWith("jl_Class", "init___jl_ScalaJSClassData")

    val DoubleModuleClass = lookupClass("jl_Double$")
    DoubleModuleClass.accessModule()
    DoubleModuleClass.callMethod("compare__D__D__I")

    val RTStringModuleClass = lookupClass("sjsr_RuntimeString$")
    RTStringModuleClass.accessModule()
    RTStringModuleClass.callMethod("hashCode__T__I")

    val RTLongClass = lookupClass(LongImpl.RuntimeLongClass)
    RTLongClass.useInstanceTests()
    RTLongClass.instantiated()
    for (method <- LongImpl.AllConstructors)
      RTLongClass.callMethod(method, statically = true)
    for (method <- LongImpl.AllMethods)
      RTLongClass.callMethod(method)

    if (reachOptimizerSymbols) {
      for (method <- LongImpl.AllIntrinsicMethods)
        RTLongClass.callMethod(method)

      // Hack to reach optional intrinsic methods only if they exist
      for (method <- LongImpl.OptionalIntrinsicMethods)
        RTLongClass.tryLookupMethod(method).foreach(_.reach(RTLongClass))
    }

    val RTLongModuleClass = lookupClass(LongImpl.RuntimeLongModuleClass)
    RTLongModuleClass.accessModule()
    for (method <- LongImpl.AllModuleMethods)
      RTLongModuleClass.callMethod(method)

    if (reachOptimizerSymbols) {
      for (hijacked <- Definitions.HijackedClasses)
        lookupClass(hijacked).instantiated()
    } else {
      for (hijacked <- Definitions.HijackedClasses)
        lookupClass(hijacked).accessData()
    }

    if (semantics.strictFloats) {
      outputMode match {
        case OutputMode.ECMAScript51Global | OutputMode.ECMAScript51Isolated =>
          val RuntimePackage = lookupClass("sjsr_package$")
          RuntimePackage.accessModule()
          RuntimePackage.callMethod("froundPolyfill__D__D")
        case OutputMode.ECMAScript6 | OutputMode.ECMAScript6StrongMode =>
          // nothing to do
      }
    }

    val BitsModuleClass = lookupClass("sjsr_Bits$")
    BitsModuleClass.accessModule()
    BitsModuleClass.callMethod("numberHashCode__D__I")
  }

  /** Reach additional class data based on reflection methods being used. */
  private def reachDataThroughReflection(): Unit = {
    val classClassInfo = _classInfos.get(Definitions.ClassClass)

    /* If Class.getSuperclass() is reachable, we can reach the data of all
     * superclasses of classes whose data we can already reach.
     */
    for {
      getSuperclassMethodInfo <-
        classClassInfo.flatMap(_.methodInfos.get("getSuperclass__jl_Class"))
      if getSuperclassMethodInfo.isReachable
    } {
      // calledFrom should always be nonEmpty if isReachable, but let's be robust
      implicit val from =
        getSuperclassMethodInfo.calledFrom.headOption.getOrElse(FromCore)
      for (classInfo <- _classInfos.values.filter(_.isDataAccessed).toList) {
        @tailrec
        def loop(classInfo: ClassInfo): Unit = {
          if (classInfo != null) {
            classInfo.accessData()
            loop(classInfo.superClass)
          }
        }
        loop(classInfo)
      }
    }
  }

  private class ClassInfo(data: Infos.ClassInfo) extends Analysis.ClassInfo {
    private[this] var _linking = false
    private[this] var _linked = false

    val encodedName = data.encodedName
    val kind = data.kind
    val isStaticModule = data.kind.hasModuleAccessor
    val isInterface = data.kind == ClassKind.Interface
    val isScalaClass = data.kind.isClass || data.kind == ClassKind.HijackedClass
    val isJSClass = data.kind.isJSClass
    val isAnyRawJSType = isJSClass || data.kind == ClassKind.RawJSType
    val isAnyClass = isScalaClass || isJSClass
    val isExported = data.isExported

    var superClass: ClassInfo = _
    var ancestors: List[ClassInfo] = _
    val descendants = mutable.ListBuffer.empty[ClassInfo]

    var nonExistent: Boolean = false

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
      for (superCls <- data.superClass)
        superClass = lookupClass(superCls)

      val parents = data.superClass ++: data.interfaces

      ancestors = this +: parents.flatMap { parent =>
        val cls = lookupClass(parent)
        cls.linkClasses()
        cls.ancestors
      }.distinct

      for (ancestor <- ancestors)
        ancestor.descendants += this
    }

    lazy val ancestorCount: Int =
      if (superClass == null) 0
      else superClass.ancestorCount + 1

    lazy val descendentClasses = descendants.filter(_.isScalaClass)

    var isInstantiated: Boolean = false
    var isAnySubclassInstantiated: Boolean = false
    var isModuleAccessed: Boolean = false
    var areInstanceTestsUsed: Boolean = false
    var isDataAccessed: Boolean = false

    var instantiatedFrom: List[From] = Nil

    val delayedCalls = mutable.Map.empty[String, From]

    def isNeededAtAll =
      areInstanceTestsUsed ||
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

    def lookupConstructor(ctorName: String): MethodInfo = {
      /* As of 0.6.6, constructors are not inherited, and so must be found
       * directly in this class. However, to be able to read sjsir files from
       * before 0.6.6, we tolerate finding it in a superclass, in which case
       * we materialize a new constructor in this class. We only allow this
       * during the initial link. In a refiner, this must not happen anymore.
       */
      methodInfos.get(ctorName).getOrElse {
        if (!allowAddingSyntheticMethods) {
          createNonExistentMethod(ctorName)
        } else {
          val inherited = lookupMethod(ctorName)
          if (inherited.owner eq this) {
            // Can happen only for non-existent constructors, at this point
            assert(inherited.nonExistent)
            inherited
          } else {
            val syntheticInfo = Infos.MethodInfo(
                encodedName = ctorName,
                methodsCalledStatically = Map(
                    superClass.encodedName -> List(ctorName)))
            val m = new MethodInfo(this, syntheticInfo)
            m.syntheticKind = MethodSyntheticKind.InheritedConstructor
            methodInfos += ctorName -> m
            m
          }
        }
      }
    }

    def lookupMethod(methodName: String): MethodInfo = {
      tryLookupMethod(methodName).getOrElse {
        createNonExistentMethod(methodName)
      }
    }

    private def createNonExistentMethod(methodName: String): MethodInfo = {
      val syntheticData = createMissingMethodInfo(methodName)
      val m = new MethodInfo(this, syntheticData)
      m.nonExistent = true
      methodInfos += methodName -> m
      m
    }

    def tryLookupMethod(methodName: String): Option[MethodInfo] = {
      assert(isScalaClass,
          s"Cannot call lookupMethod($methodName) on non Scala class $this")
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

    def tryLookupReflProxyMethod(proxyName: String): Option[MethodInfo] = {
      if (!allowAddingSyntheticMethods) {
        tryLookupMethod(proxyName)
      } else {
        /* The lookup for a target method in this code implements the
         * algorithm defining `java.lang.Class.getMethod`. This mimics how
         * reflective calls are implemented on the JVM, at link time.
         *
         * Caveat: protected methods are not ignored. This can only make an
         * otherwise invalid reflective call suddenly able to call a protected
         * method. It never breaks valid reflective calls. This could be fixed
         * if the IR retained the information that a method is protected.
         */

        @tailrec
        def loop(ancestorInfo: ClassInfo): Option[MethodInfo] = {
          if (ancestorInfo ne null) {
            ancestorInfo.methodInfos.get(proxyName) match {
              case Some(m) =>
                assert(m.isReflProxy && !m.isAbstract)
                Some(m)

              case _ =>
                ancestorInfo.findProxyMatch(proxyName) match {
                  case Some(target) =>
                    val targetName = target.encodedName
                    Some(ancestorInfo.createReflProxy(proxyName, targetName))

                  case None =>
                    loop(ancestorInfo.superClass)
                }
            }
          } else {
            None
          }
        }

        loop(this)
      }
    }

    private def findProxyMatch(proxyName: String): Option[MethodInfo] = {
      val candidates = methodInfos.valuesIterator.filter { m =>
        // TODO In theory we should filter out protected methods
        !m.isReflProxy && !m.isExported && !m.isAbstract &&
        reflProxyMatches(m.encodedName, proxyName)
      }.toSeq

      /* From the JavaDoc of java.lang.Class.getMethod:
       *
       *   If more than one [candidate] method is found in C, and one of these
       *   methods has a return type that is more specific than any of the
       *   others, that method is reflected; otherwise one of the methods is
       *   chosen arbitrarily.
       */

      val targets = candidates.filterNot { c =>
        val resultType = methodResultType(c.encodedName)
        candidates.exists { other =>
          (other ne c) &&
          isMoreSpecific(methodResultType(other.encodedName), resultType)
        }
      }

      /* This last step (chosen arbitrarily) causes some soundness issues of
       * the implementation of reflective calls. This is bug-compatible with
       * Scala/JVM.
       */
      targets.headOption
    }

    private def reflProxyMatches(methodName: String, proxyName: String): Boolean = {
      val sepPos = methodName.lastIndexOf("__")
      sepPos >= 0 && methodName.substring(0, sepPos + 2) == proxyName
    }

    private def methodResultType(methodName: String): ir.Types.ReferenceType = {
      val typeName = methodName.substring(methodName.lastIndexOf("__") + 2)
      val arrayDepth = typeName.indexWhere(_ != 'A')
      if (arrayDepth == 0)
        ir.Types.ClassType(typeName)
      else
        ir.Types.ArrayType(typeName.substring(arrayDepth), arrayDepth)
    }

    private def isMoreSpecific(left: ir.Types.ReferenceType,
        right: ir.Types.ReferenceType): Boolean = {
      import ir.Types._

      def classIsMoreSpecific(leftCls: String, rightCls: String): Boolean = {
        leftCls != rightCls && {
          val leftInfo = _classInfos.get(leftCls)
          val rightInfo = _classInfos.get(rightCls)
          leftInfo.zip(rightInfo).exists { case (l, r) =>
            l.ancestors.contains(r)
          }
        }
      }

      (left, right) match {
        case (ClassType(leftCls), ClassType(rightCls)) =>
          classIsMoreSpecific(leftCls, rightCls)
        case (ArrayType(leftBase, leftDepth), ArrayType(rightBase, rightDepth)) =>
          leftDepth == rightDepth && classIsMoreSpecific(leftBase, rightBase)
        case (ArrayType(_, _), ClassType(ObjectClass)) =>
          true
        case _ =>
          false
      }
    }

    private def createReflProxy(proxyName: String,
        targetName: String): MethodInfo = {
      assert(this.isScalaClass,
          s"Cannot create reflective proxy in non-Scala class $this")

      val returnsChar = targetName.endsWith("__C")
      val syntheticInfo = Infos.MethodInfo(
          encodedName = proxyName,
          methodsCalled = Map(
              this.encodedName -> List(targetName)),
          methodsCalledStatically = (
              if (returnsChar) Map(BoxedCharacterClass -> List("init___C"))
              else Map.empty),
          instantiatedClasses = (
              if (returnsChar) List(BoxedCharacterClass)
              else Nil))
      val m = new MethodInfo(this, syntheticInfo)
      m.syntheticKind = MethodSyntheticKind.ReflectiveProxy(targetName)
      methodInfos += proxyName -> m
      m
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
      if (!isJSClass) {
        for (methodInfo <- methodInfos.values) {
          if (methodInfo.isExported)
            callMethod(methodInfo.encodedName)
        }
      }
    }

    def accessModule()(implicit from: From): Unit = {
      if (!isStaticModule) {
        _errors += NotAModule(this, from)
      } else if (!isModuleAccessed) {
        isModuleAccessed = true
        instantiated()
        if (isScalaClass)
          callMethod("init___", statically = true)
      }
    }

    def instantiated()(implicit from: From): Unit = {
      instantiatedFrom ::= from
      if (!isInstantiated && (isScalaClass || isAnyRawJSType)) {
        isInstantiated = true

        if (isScalaClass) {
          accessData()
          ancestors.foreach(_.subclassInstantiated())

          for ((methodName, from) <- delayedCalls)
            delayedCallMethod(methodName)(from)
        } else {
          assert(isAnyRawJSType)

          subclassInstantiated()

          if (isJSClass)
            superClass.instantiated()

          for (methodInfo <- methodInfos.values) {
            if (methodInfo.isExported)
              methodInfo.reach(this)(FromExports)
          }
        }
      }
    }

    private def subclassInstantiated()(implicit from: From): Unit = {
      instantiatedFrom ::= from
      if (!isAnySubclassInstantiated && (isScalaClass || isAnyRawJSType)) {
        isAnySubclassInstantiated = true
      }
    }

    def useInstanceTests()(implicit from: From): Unit = {
      if (!areInstanceTestsUsed) {
        checkExistent()
        areInstanceTestsUsed = true
      }
    }

    def accessData()(implicit from: From): Unit = {
      if (!isDataAccessed) {
        checkExistent()
        isDataAccessed = true
      }
    }

    def checkExistent()(implicit from: From): Unit = {
      if (nonExistent) {
        _errors += MissingClass(this, from)
        _allAvailable = false
      }
    }

    def callMethod(methodName: String, statically: Boolean = false)(
        implicit from: From): Unit = {
      if (isConstructorName(methodName)) {
        // constructors must always be called statically
        assert(statically,
            s"Trying to call dynamically the constructor $this.$methodName from $from")
        lookupConstructor(methodName).reachStatic()
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

    private def delayedCallMethod(methodName: String)(implicit from: From): Unit = {
      if (isReflProxyName(methodName)) {
        tryLookupReflProxyMethod(methodName).foreach(_.reach(this))
      } else {
        lookupMethod(methodName).reach(this)
      }
    }

    def callStaticMethod(methodName: String)(implicit from: From): Unit = {
      lookupStaticMethod(methodName).reachStatic()
    }
  }

  private class MethodInfo(val owner: ClassInfo,
      data: Infos.MethodInfo) extends Analysis.MethodInfo {

    val encodedName = data.encodedName
    val isStatic = data.isStatic
    val isAbstract = data.isAbstract
    val isExported = data.isExported
    val isReflProxy = isReflProxyName(encodedName)

    var isReachable: Boolean = false

    var calledFrom: List[From] = Nil
    var instantiatedSubclasses: List[ClassInfo] = Nil

    var nonExistent: Boolean = false

    var syntheticKind: MethodSyntheticKind = MethodSyntheticKind.None

    override def toString(): String = s"$owner.$encodedName"

    def reachStatic()(implicit from: From): Unit = {
      assert(!isAbstract,
          s"Trying to reach statically the abstract method $this")

      checkExistent()

      calledFrom ::= from
      if (!isReachable) {
        isReachable = true
        doReach()
      }
    }

    def reach(inClass: ClassInfo)(implicit from: From): Unit = {
      assert(!isStatic,
          s"Trying to dynamically reach the static method $this")
      assert(!isAbstract,
          s"Trying to dynamically reach the abstract method $this")
      assert(owner.isAnyClass,
          s"Trying to dynamically reach the non-class method $this")
      assert(!isConstructorName(encodedName),
          s"Trying to dynamically reach the constructor $this")

      checkExistent()

      calledFrom ::= from
      instantiatedSubclasses ::= inClass

      if (!isReachable) {
        isReachable = true
        doReach()
      }
    }

    private def checkExistent()(implicit from: From) = {
      if (nonExistent) {
        _errors += MissingMethod(this, from)
        _allAvailable = false
      }
    }

    private[this] def doReach(): Unit = {
      implicit val from = FromMethod(this)

      for (moduleName <- data.accessedModules) {
        lookupClass(moduleName).accessModule()
      }

      for (className <- data.instantiatedClasses) {
        lookupClass(className).instantiated()
      }

      for (className <- data.usedInstanceTests) {
        if (!Definitions.PrimitiveClasses.contains(className))
          lookupClass(className).useInstanceTests()
      }

      for (className <- data.accessedClassData) {
        if (!Definitions.PrimitiveClasses.contains(className))
          lookupClass(className).accessData()
      }

      /* `for` loops on maps are written with `while` loops to help the JIT
       * compiler to inline and stack allocate tupples created by the iterators
       */
      val methodsCalledIterator = data.methodsCalled.iterator
      while (methodsCalledIterator.hasNext) {
        val (className, methods) = methodsCalledIterator.next()
        if (className == Definitions.PseudoArrayClass) {
          /* The pseudo Array class is not reified in our analyzer/analysis,
           * so we need to cheat here.
           * In the Array[T] class family, only clone__O is defined and
           * overrides j.l.Object.clone__O. Since this method is implemented
           * in scalajsenv.js and always kept, we can ignore it.
           * All other methods resolve to their definition in Object, so we
           * can model their reachability by calling them statically in the
           * Object class.
           */
          val objectClass = lookupClass(Definitions.ObjectClass)
          for (methodName <- methods) {
            if (methodName != "clone__O")
              objectClass.callMethod(methodName, statically = true)
          }
        } else {
          val classInfo = lookupClass(className)
          for (methodName <- methods)
            classInfo.callMethod(methodName)
        }
      }

      val methodsCalledStaticallyIterator = data.methodsCalledStatically.iterator
      while (methodsCalledStaticallyIterator.hasNext) {
        val (className, methods) = methodsCalledStaticallyIterator.next()
        val classInfo = lookupClass(className)
        for (methodName <- methods)
          classInfo.callMethod(methodName, statically = true)
      }

      val staticMethodsCalledIterator = data.staticMethodsCalled.iterator
      while (staticMethodsCalledIterator.hasNext) {
        val (className, methods) = staticMethodsCalledIterator.next()
        val classInfo = lookupClass(className)
        for (methodName <- methods)
          classInfo.callStaticMethod(methodName)
      }
    }
  }

  private def createMissingClassInfo(encodedName: String): Infos.ClassInfo = {
    // We create a module class to avoid cascading errors
    Infos.ClassInfo(
        encodedName = encodedName,
        isExported = false,
        kind = ClassKind.ModuleClass,
        superClass = Some("O"),
        interfaces = Nil,
        methods = List(
            createMissingMethodInfo("init___"))
    )
  }

  private def createMissingMethodInfo(encodedName: String,
      isStatic: Boolean = false,
      isAbstract: Boolean = false): Infos.MethodInfo = {
    Infos.MethodInfo(encodedName = encodedName,
        isStatic = isStatic, isAbstract = isAbstract)
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
