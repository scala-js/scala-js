/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js IR                **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.ir

import scala.collection.mutable

import Definitions._
import Trees._
import Types._

object Infos {

  final class ClassInfo private (
      val encodedName: String,
      val isExported: Boolean,
      val kind: ClassKind,
      val superClass: Option[String], // always None for interfaces
      val interfaces: List[String], // direct parent interfaces only
      val methods: List[MethodInfo]
  )

  object ClassInfo {
    def apply(
        encodedName: String,
        isExported: Boolean = false,
        kind: ClassKind = ClassKind.Class,
        superClass: Option[String] = None,
        interfaces: List[String] = Nil,
        methods: List[MethodInfo] = Nil): ClassInfo = {
      new ClassInfo(encodedName, isExported, kind, superClass,
          interfaces, methods)
    }
  }

  final class MethodInfo private (
      val encodedName: String,
      val isStatic: Boolean,
      val isAbstract: Boolean,
      val isExported: Boolean,
      val methodsCalled: Map[String, List[String]],
      val methodsCalledStatically: Map[String, List[String]],
      val staticMethodsCalled: Map[String, List[String]],
      /** For a Scala class, it is instantiated with a `New`; for a JS class,
       *  its constructor is accessed with a `JSLoadConstructor`.
       */
      val instantiatedClasses: List[String],
      val accessedModules: List[String],
      val usedInstanceTests: List[String],
      val accessedClassData: List[String]
  )

  object MethodInfo {
    def apply(
        encodedName: String,
        isStatic: Boolean = false,
        isAbstract: Boolean = false,
        isExported: Boolean = false,
        methodsCalled: Map[String, List[String]] = Map.empty,
        methodsCalledStatically: Map[String, List[String]] = Map.empty,
        staticMethodsCalled: Map[String, List[String]] = Map.empty,
        instantiatedClasses: List[String] = Nil,
        accessedModules: List[String] = Nil,
        usedInstanceTests: List[String] = Nil,
        accessedClassData: List[String] = Nil): MethodInfo = {
      new MethodInfo(encodedName, isStatic, isAbstract, isExported,
          methodsCalled, methodsCalledStatically, staticMethodsCalled,
          instantiatedClasses, accessedModules, usedInstanceTests,
          accessedClassData)
    }
  }

  final class ClassInfoBuilder {
    private var encodedName: String = ""
    private var kind: ClassKind = ClassKind.Class
    private var isExported: Boolean = false
    private var superClass: Option[String] = None
    private val interfaces = mutable.ListBuffer.empty[String]
    private val methods = mutable.ListBuffer.empty[MethodInfo]

    def setEncodedName(encodedName: String): this.type = {
      this.encodedName = encodedName
      this
    }

    def setKind(kind: ClassKind): this.type = {
      this.kind = kind
      this
    }

    def setIsExported(isExported: Boolean): this.type = {
      this.isExported = isExported
      this
    }

    def setSuperClass(superClass: Option[String]): this.type = {
      this.superClass = superClass
      this
    }

    def addInterface(interface: String): this.type = {
      interfaces += interface
      this
    }

    def addInterfaces(interfaces: TraversableOnce[String]): this.type = {
      this.interfaces ++= interfaces
      this
    }

    def addMethod(methodInfo: MethodInfo): this.type = {
      methods += methodInfo
      this
    }

    def result(): ClassInfo = {
      ClassInfo(encodedName, isExported, kind, superClass,
          interfaces.toList, methods.toList)
    }
  }

  final class MethodInfoBuilder {
    private var encodedName: String = ""
    private var isStatic: Boolean = false
    private var isAbstract: Boolean = false
    private var isExported: Boolean = false

    private val methodsCalled = mutable.Map.empty[String, mutable.Set[String]]
    private val methodsCalledStatically = mutable.Map.empty[String, mutable.Set[String]]
    private val staticMethodsCalled = mutable.Map.empty[String, mutable.Set[String]]
    private val instantiatedClasses = mutable.Set.empty[String]
    private val accessedModules = mutable.Set.empty[String]
    private val usedInstanceTests = mutable.Set.empty[String]
    private val accessedClassData = mutable.Set.empty[String]

    def setEncodedName(encodedName: String): this.type = {
      this.encodedName = encodedName
      this
    }

    def setIsStatic(isStatic: Boolean): this.type = {
      this.isStatic = isStatic
      this
    }

    def setIsAbstract(isAbstract: Boolean): this.type = {
      this.isAbstract = isAbstract
      this
    }

    def setIsExported(isExported: Boolean): this.type = {
      this.isExported = isExported
      this
    }

    def addMethodCalled(receiverTpe: Type, method: String): this.type = {
      receiverTpe match {
        case ClassType(cls)  => addMethodCalled(cls, method)
        case AnyType         => addMethodCalled(ObjectClass, method)
        case UndefType       => addMethodCalled(BoxedUnitClass, method)
        case BooleanType     => addMethodCalled(BoxedBooleanClass, method)
        case IntType         => addMethodCalled(BoxedIntegerClass, method)
        case LongType        => addMethodCalled(BoxedLongClass, method)
        case FloatType       => addMethodCalled(BoxedFloatClass, method)
        case DoubleType      => addMethodCalled(BoxedDoubleClass, method)
        case StringType      => addMethodCalled(StringClass, method)
        case ArrayType(_, _) => addMethodCalled(PseudoArrayClass, method)

        case NullType | NothingType =>
          // Nothing to do

        case NoType | RecordType(_) =>
          throw new IllegalArgumentException(
              s"Illegal receiver type: $receiverTpe")
      }

      this
    }

    def addMethodCalled(cls: String, method: String): this.type = {
      methodsCalled.getOrElseUpdate(cls, mutable.Set.empty) += method
      this
    }

    def addMethodCalledStatically(cls: String, method: String): this.type = {
      methodsCalledStatically.getOrElseUpdate(cls, mutable.Set.empty) += method
      this
    }

    def addStaticMethodCalled(cls: String, method: String): this.type = {
      staticMethodsCalled.getOrElseUpdate(cls, mutable.Set.empty) += method
      this
    }

    def addInstantiatedClass(cls: String): this.type = {
      instantiatedClasses += cls
      this
    }

    def addInstantiatedClass(cls: String, ctor: String): this.type =
      addInstantiatedClass(cls).addMethodCalledStatically(cls, ctor)

    def addAccessedModule(cls: String): this.type = {
      accessedModules += cls
      this
    }

    def addUsedInstanceTest(tpe: ReferenceType): this.type =
      addUsedInstanceTest(baseNameOf(tpe))

    def addUsedInstanceTest(cls: String): this.type = {
      usedInstanceTests += cls
      this
    }

    def addAccessedClassData(tpe: ReferenceType): this.type =
      addAccessedClassData(baseNameOf(tpe))

    def addAccessedClassData(cls: String): this.type = {
      accessedClassData += cls
      this
    }

    private def baseNameOf(tpe: ReferenceType): String = tpe match {
      case ClassType(name)    => name
      case ArrayType(base, _) => base
    }

    def result(): MethodInfo = {
      MethodInfo(
          encodedName = encodedName,
          isStatic = isStatic,
          isAbstract = isAbstract,
          isExported = isExported,
          methodsCalled = methodsCalled.toMap.mapValues(_.toList),
          methodsCalledStatically = methodsCalledStatically.toMap.mapValues(_.toList),
          staticMethodsCalled = staticMethodsCalled.toMap.mapValues(_.toList),
          instantiatedClasses = instantiatedClasses.toList,
          accessedModules = accessedModules.toList,
          usedInstanceTests = usedInstanceTests.toList,
          accessedClassData = accessedClassData.toList
      )
    }
  }

  /** Generates the [[ClassInfo]] of a [[Trees.ClassDef]]. */
  def generateClassInfo(classDef: ClassDef): ClassInfo = {
    val builder = new ClassInfoBuilder()
      .setEncodedName(classDef.name.name)
      .setKind(classDef.kind)
      .setSuperClass(classDef.superClass.map(_.name))
      .addInterfaces(classDef.interfaces.map(_.name))

    var exportedConstructors: List[ConstructorExportDef] = Nil

    classDef.defs foreach {
      case methodDef: MethodDef =>
        builder.addMethod(generateMethodInfo(methodDef))
      case propertyDef: PropertyDef =>
        builder.addMethod(generatePropertyInfo(propertyDef))
      case constructorDef: ConstructorExportDef =>
        builder.setIsExported(true)
        exportedConstructors ::= constructorDef
      case _:JSClassExportDef | _:ModuleExportDef =>
        builder.setIsExported(true)
      case _ =>
    }

    if (exportedConstructors.nonEmpty)
      builder.addMethod(generateExportedConstructorsInfo(exportedConstructors))

    builder.result()
  }

  /** Generates the [[MethodInfo]] of a [[Trees.MethodDef]]. */
  def generateMethodInfo(methodDef: MethodDef): MethodInfo =
    new GenInfoTraverser().generateMethodInfo(methodDef)

  /** Generates the [[MethodInfo]] of a [[Trees.PropertyDef]]. */
  def generatePropertyInfo(propertyDef: PropertyDef): MethodInfo =
    new GenInfoTraverser().generatePropertyInfo(propertyDef)

  /** Generates the [[MethodInfo]] of a list of [[Trees.ConstructorExportDef]]s. */
  def generateExportedConstructorsInfo(
      constructorDefs: List[ConstructorExportDef]): MethodInfo = {
    new GenInfoTraverser().generateExportedConstructorsInfo(constructorDefs)
  }

  private final class GenInfoTraverser extends Traversers.Traverser {
    private val builder = new MethodInfoBuilder

    def generateMethodInfo(methodDef: MethodDef): MethodInfo = {
      builder
        .setEncodedName(methodDef.name.name)
        .setIsStatic(methodDef.static)
        .setIsAbstract(methodDef.body == EmptyTree)
        .setIsExported(methodDef.name.isInstanceOf[StringLiteral])

      // body can be EmptyTree, but that's fine
      traverse(methodDef.body)

      builder.result()
    }

    def generatePropertyInfo(propertyDef: PropertyDef): MethodInfo = {
      builder
        .setEncodedName(propertyDef.name.name)
        .setIsExported(true)

      // Any of getterBody and setterBody can be EmptyTree, but that's fine
      traverse(propertyDef.getterBody)
      traverse(propertyDef.setterBody)

      builder.result()
    }

    def generateExportedConstructorsInfo(
        constructorDefs: List[ConstructorExportDef]): MethodInfo = {
      builder
        .setEncodedName(ExportedConstructorsName)
        .setIsExported(true)

      for (constructorDef <- constructorDefs)
        traverse(constructorDef.body)

      builder.result()
    }

    override def traverse(tree: Tree): Unit = {
      tree match {
        case New(ClassType(cls), ctor, _) =>
          builder.addInstantiatedClass(cls, ctor.name)

        case Apply(receiver, Ident(method, _), _) =>
          builder.addMethodCalled(receiver.tpe, method)
        case ApplyStatically(_, ClassType(cls), method, _) =>
          builder.addMethodCalledStatically(cls, method.name)
        case ApplyStatic(ClassType(cls), method, _) =>
          builder.addStaticMethodCalled(cls, method.name)

        case LoadModule(ClassType(cls)) =>
          builder.addAccessedModule(cls)

        case IsInstanceOf(_, tpe) =>
          builder.addUsedInstanceTest(tpe)
        case AsInstanceOf(_, tpe) =>
          builder.addUsedInstanceTest(tpe)

        case NewArray(tpe, _) =>
          builder.addAccessedClassData(tpe)
        case ArrayValue(tpe, _) =>
          builder.addAccessedClassData(tpe)
        case ClassOf(cls) =>
          builder.addAccessedClassData(cls)

        case LoadJSConstructor(cls) =>
          builder.addInstantiatedClass(cls.className)

        case LoadJSModule(ClassType(cls)) =>
          builder.addAccessedModule(cls)

        case _ =>
      }

      super.traverse(tree)
    }
  }

}
