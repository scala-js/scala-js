/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.linker.analyzer

import scala.collection.mutable

import org.scalajs.ir.ClassKind
import org.scalajs.ir.Names._
import org.scalajs.ir.Traversers._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.linker.backend.emitter.Transients._

object Infos {

  private val StringArgConstructorName =
    MethodName.constructor(List(ClassRef(BoxedStringClass)))

  private val cloneMethodName = MethodName("clone", Nil, ClassRef(ObjectClass))

  final case class NamespacedMethodName(
      namespace: MemberNamespace, methodName: MethodName)

  final class ClassInfo private (
      val className: ClassName,
      val isExported: Boolean,
      val kind: ClassKind,
      val superClass: Option[ClassName], // always None for interfaces
      val interfaces: List[ClassName], // direct parent interfaces only
      val referencedFieldClasses: List[ClassName],
      val methods: List[MethodInfo],
      val exportedMembers: List[ReachabilityInfo],
      val topLevelExportedMembers: List[ReachabilityInfo],
      val topLevelExportNames: List[String]
  ) {
    override def toString(): String = className.nameString
  }

  object ClassInfo {
    def apply(
        className: ClassName,
        isExported: Boolean,
        kind: ClassKind,
        superClass: Option[ClassName],
        interfaces: List[ClassName],
        referencedFieldClasses: List[ClassName],
        methods: List[MethodInfo],
        exportedMembers: List[ReachabilityInfo],
        topLevelExportedMembers: List[ReachabilityInfo],
        topLevelExportNames: List[String]): ClassInfo = {
      new ClassInfo(className, isExported, kind, superClass,
          interfaces, referencedFieldClasses, methods, exportedMembers,
          topLevelExportedMembers, topLevelExportNames)
    }
  }

  final class MethodInfo private (
      val methodName: MethodName,
      val namespace: MemberNamespace,
      val isAbstract: Boolean,
      val reachabilityInfo: ReachabilityInfo
  ) {
    override def toString(): String = methodName.nameString
  }

  object MethodInfo {
    def apply(
        methodName: MethodName,
        namespace: MemberNamespace,
        isAbstract: Boolean,
        reachabilityInfo: ReachabilityInfo): MethodInfo = {
      new MethodInfo(methodName, namespace, isAbstract, reachabilityInfo)
    }
  }

  final class ReachabilityInfo private (
      val privateJSFieldsUsed: Map[ClassName, List[FieldName]],
      val staticFieldsRead: Map[ClassName, List[FieldName]],
      val staticFieldsWritten: Map[ClassName, List[FieldName]],
      val methodsCalled: Map[ClassName, List[MethodName]],
      val methodsCalledStatically: Map[ClassName, List[NamespacedMethodName]],
      /** For a Scala class, it is instantiated with a `New`; for a JS class,
       *  its constructor is accessed with a `JSLoadConstructor`.
       */
      val instantiatedClasses: List[ClassName],
      val accessedModules: List[ClassName],
      val usedInstanceTests: List[ClassName],
      val accessedClassData: List[ClassName],
      val referencedClasses: List[ClassName]
  )

  object ReachabilityInfo {
    val Empty: ReachabilityInfo = {
      ReachabilityInfo(
          Map.empty, Map.empty, Map.empty, Map.empty, Map.empty,
          Nil, Nil, Nil, Nil, Nil)
    }

    def apply(
        privateJSFieldsUsed: Map[ClassName, List[FieldName]],
        staticFieldsRead: Map[ClassName, List[FieldName]],
        staticFieldsWritten: Map[ClassName, List[FieldName]],
        methodsCalled: Map[ClassName, List[MethodName]],
        methodsCalledStatically: Map[ClassName, List[NamespacedMethodName]],
        instantiatedClasses: List[ClassName],
        accessedModules: List[ClassName],
        usedInstanceTests: List[ClassName],
        accessedClassData: List[ClassName],
        referencedClasses: List[ClassName]): ReachabilityInfo = {
      new ReachabilityInfo(privateJSFieldsUsed, staticFieldsRead,
          staticFieldsWritten, methodsCalled, methodsCalledStatically,
          instantiatedClasses, accessedModules, usedInstanceTests,
          accessedClassData, referencedClasses)
    }
  }

  final class ClassInfoBuilder(private val className: ClassName) {
    private var kind: ClassKind = ClassKind.Class
    private var isExported: Boolean = false
    private var superClass: Option[ClassName] = None
    private val interfaces = mutable.ListBuffer.empty[ClassName]
    private val referencedFieldClasses = mutable.Set.empty[ClassName]
    private val methods = mutable.ListBuffer.empty[MethodInfo]
    private val exportedMembers = mutable.ListBuffer.empty[ReachabilityInfo]
    private val topLevelExportedMembers = mutable.ListBuffer.empty[ReachabilityInfo]
    private var topLevelExportNames: List[String] = Nil

    def setKind(kind: ClassKind): this.type = {
      this.kind = kind
      this
    }

    def setIsExported(isExported: Boolean): this.type = {
      this.isExported = isExported
      this
    }

    def setSuperClass(superClass: Option[ClassName]): this.type = {
      this.superClass = superClass
      this
    }

    def addInterface(interface: ClassName): this.type = {
      interfaces += interface
      this
    }

    def addInterfaces(interfaces: List[ClassName]): this.type = {
      this.interfaces ++= interfaces
      this
    }

    def maybeAddReferencedFieldClass(tpe: Type): this.type = {
      tpe match {
        case ClassType(cls) =>
          referencedFieldClasses += cls
        case ArrayType(ArrayTypeRef(ClassRef(cls), _)) =>
          referencedFieldClasses += cls
        case _ =>
      }

      this
    }

    def addMethod(methodInfo: MethodInfo): this.type = {
      methods += methodInfo
      this
    }

    def addExportedMember(reachabilityInfo: ReachabilityInfo): this.type = {
      exportedMembers += reachabilityInfo
      this
    }

    def addTopLevelExportedMember(reachabilityInfo: ReachabilityInfo): this.type = {
      topLevelExportedMembers += reachabilityInfo
      this
    }

    def setTopLevelExportNames(names: List[String]): this.type = {
      topLevelExportNames = names
      this
    }

    def result(): ClassInfo = {
      ClassInfo(className, isExported, kind, superClass,
          interfaces.toList, referencedFieldClasses.toList, methods.toList,
          exportedMembers.toList, topLevelExportedMembers.toList,
          topLevelExportNames)
    }
  }

  final class ReachabilityInfoBuilder {
    private val privateJSFieldsUsed = mutable.Map.empty[ClassName, mutable.Set[FieldName]]
    private val staticFieldsRead = mutable.Map.empty[ClassName, mutable.Set[FieldName]]
    private val staticFieldsWritten = mutable.Map.empty[ClassName, mutable.Set[FieldName]]
    private val methodsCalled = mutable.Map.empty[ClassName, mutable.Set[MethodName]]
    private val methodsCalledStatically = mutable.Map.empty[ClassName, mutable.Set[NamespacedMethodName]]
    private val instantiatedClasses = mutable.Set.empty[ClassName]
    private val accessedModules = mutable.Set.empty[ClassName]
    private val usedInstanceTests = mutable.Set.empty[ClassName]
    private val accessedClassData = mutable.Set.empty[ClassName]
    private val referencedClasses = mutable.Set.empty[ClassName]

    def addPrivateJSFieldUsed(cls: ClassName, field: FieldName): this.type = {
      privateJSFieldsUsed.getOrElseUpdate(cls, mutable.Set.empty) += field
      this
    }

    def addStaticFieldRead(cls: ClassName, field: FieldName): this.type = {
      staticFieldsRead.getOrElseUpdate(cls, mutable.Set.empty) += field
      this
    }

    def addStaticFieldWritten(cls: ClassName, field: FieldName): this.type = {
      staticFieldsWritten.getOrElseUpdate(cls, mutable.Set.empty) += field
      this
    }

    def addMethodCalled(receiverTpe: Type, method: MethodName): this.type = {
      receiverTpe match {
        case ClassType(cls) => addMethodCalled(cls, method)
        case AnyType        => addMethodCalled(ObjectClass, method)
        case UndefType      => addMethodCalled(BoxedUnitClass, method)
        case BooleanType    => addMethodCalled(BoxedBooleanClass, method)
        case CharType       => addMethodCalled(BoxedCharacterClass, method)
        case ByteType       => addMethodCalled(BoxedByteClass, method)
        case ShortType      => addMethodCalled(BoxedShortClass, method)
        case IntType        => addMethodCalled(BoxedIntegerClass, method)
        case LongType       => addMethodCalled(BoxedLongClass, method)
        case FloatType      => addMethodCalled(BoxedFloatClass, method)
        case DoubleType     => addMethodCalled(BoxedDoubleClass, method)
        case StringType     => addMethodCalled(BoxedStringClass, method)

        case ArrayType(_) =>
          /* The pseudo Array class is not reified in our analyzer/analysis,
           * so we need to cheat here.
           * In the Array[T] class family, only clone()Object is defined and
           * overrides j.l.Object.clone()Object. Since this method is
           * implemented in CoreJSLib and always kept, we can ignore it.
           * All other methods resolve to their definition in Object, so we
           * can model their reachability by calling them statically in the
           * Object class.
           */
          if (method != cloneMethodName) {
            addMethodCalledStatically(ObjectClass,
                NamespacedMethodName(MemberNamespace.Public, method))
          }

        case NullType | NothingType =>
          // Nothing to do

        case NoType | RecordType(_) =>
          throw new IllegalArgumentException(
              s"Illegal receiver type: $receiverTpe")
      }

      this
    }

    def addMethodCalled(cls: ClassName, method: MethodName): this.type = {
      methodsCalled.getOrElseUpdate(cls, mutable.Set.empty) += method
      this
    }

    def addMethodCalledStatically(cls: ClassName,
        method: NamespacedMethodName): this.type = {
      methodsCalledStatically.getOrElseUpdate(cls, mutable.Set.empty) += method
      this
    }

    def addInstantiatedClass(cls: ClassName): this.type = {
      instantiatedClasses += cls
      this
    }

    def addInstantiatedClass(cls: ClassName, ctor: MethodName): this.type = {
      addInstantiatedClass(cls).addMethodCalledStatically(cls,
          NamespacedMethodName(MemberNamespace.Constructor, ctor))
    }

    def addAccessedModule(cls: ClassName): this.type = {
      accessedModules += cls
      this
    }

    def maybeAddUsedInstanceTest(tpe: Type): this.type = {
      tpe match {
        case ClassType(className) =>
          addUsedInstanceTest(className)
        case ArrayType(ArrayTypeRef(ClassRef(baseClassName), _)) =>
          addUsedInstanceTest(baseClassName)
        case _ =>
      }
      this
    }

    def addUsedInstanceTest(cls: ClassName): this.type = {
      usedInstanceTests += cls
      this
    }

    def maybeAddAccessedClassData(typeRef: TypeRef): this.type = {
      typeRef match {
        case ClassRef(cls) =>
          addAccessedClassData(cls)
        case ArrayTypeRef(ClassRef(cls), _) =>
          addAccessedClassData(cls)
        case _ =>
      }
      this
    }

    def addAccessedClassData(cls: ClassName): this.type = {
      accessedClassData += cls
      this
    }

    def maybeAddReferencedClass(typeRef: TypeRef): this.type = {
      typeRef match {
        case ClassRef(cls) =>
          addReferencedClass(cls)
        case ArrayTypeRef(ClassRef(cls), _) =>
          addReferencedClass(cls)
        case _ =>
      }
      this
    }

    def addReferencedClass(cls: ClassName): this.type = {
      referencedClasses += cls
      this
    }

    def maybeAddReferencedClass(tpe: Type): this.type = {
      tpe match {
        case ClassType(cls) =>
          addReferencedClass(cls)
        case ArrayType(ArrayTypeRef(ClassRef(cls), _)) =>
          addReferencedClass(cls)
        case _ =>
      }
      this
    }

    def result(): ReachabilityInfo = {
      def toMapOfLists[A, B](m: mutable.Map[A, mutable.Set[B]]): Map[A, List[B]] =
        m.map(kv => kv._1 -> kv._2.toList).toMap

      ReachabilityInfo(
          privateJSFieldsUsed = toMapOfLists(privateJSFieldsUsed),
          staticFieldsRead = toMapOfLists(staticFieldsRead),
          staticFieldsWritten = toMapOfLists(staticFieldsWritten),
          methodsCalled = toMapOfLists(methodsCalled),
          methodsCalledStatically = toMapOfLists(methodsCalledStatically),
          instantiatedClasses = instantiatedClasses.toList,
          accessedModules = accessedModules.toList,
          usedInstanceTests = usedInstanceTests.toList,
          accessedClassData = accessedClassData.toList,
          referencedClasses = referencedClasses.toList
      )
    }
  }

  /** Generates the [[ClassInfo]] of a
   *  [[org.scalajs.ir.Trees.ClassDef Trees.ClassDef]].
   */
  def generateClassInfo(classDef: ClassDef): ClassInfo = {
    val builder = new ClassInfoBuilder(classDef.name.name)
      .setKind(classDef.kind)
      .setSuperClass(classDef.superClass.map(_.name))
      .addInterfaces(classDef.interfaces.map(_.name))

    classDef.memberDefs foreach {
      case fieldDef: AnyFieldDef =>
        builder.maybeAddReferencedFieldClass(fieldDef.ftpe)

      case methodDef: MethodDef =>
        builder.addMethod(generateMethodInfo(methodDef))

      case methodDef: JSMethodDef =>
        builder.addExportedMember(generateJSMethodInfo(methodDef))

      case propertyDef: JSPropertyDef =>
        builder.addExportedMember(generateJSPropertyInfo(propertyDef))
    }

    if (classDef.topLevelExportDefs.nonEmpty) {
      builder.setIsExported(true)

      val optInfo = generateTopLevelExportsInfo(classDef.name.name,
          classDef.topLevelExportDefs)
      optInfo.foreach(builder.addTopLevelExportedMember(_))

      val names = classDef.topLevelExportDefs.map(_.topLevelExportName)
      builder.setTopLevelExportNames(names)
    }

    builder.result()
  }

  /** Generates the [[MethodInfo]] of a
   *  [[org.scalajs.ir.Trees.MethodDef Trees.MethodDef]].
   */
  def generateMethodInfo(methodDef: MethodDef): MethodInfo =
    new GenInfoTraverser().generateMethodInfo(methodDef)

  /** Generates the [[ReachabilityInfo]] of a
   *  [[org.scalajs.ir.Trees.JSMethodDef Trees.JSMethodDef]].
   */
  def generateJSMethodInfo(methodDef: JSMethodDef): ReachabilityInfo =
    new GenInfoTraverser().generateJSMethodInfo(methodDef)

  /** Generates the [[ReachabilityInfo]] of a
   *  [[org.scalajs.ir.Trees.JSPropertyDef Trees.JSPropertyDef]].
   */
  def generateJSPropertyInfo(propertyDef: JSPropertyDef): ReachabilityInfo =
    new GenInfoTraverser().generateJSPropertyInfo(propertyDef)

  /** Generates the [[MethodInfo]] for the top-level exports. */
  def generateTopLevelExportsInfo(enclosingClass: ClassName,
      topLevelExportDefs: List[TopLevelExportDef]): Option[ReachabilityInfo] = {

    var topLevelMethodExports: List[TopLevelMethodExportDef] = Nil
    var topLevelFieldExports: List[TopLevelFieldExportDef] = Nil

    topLevelExportDefs.foreach {
      case _:TopLevelJSClassExportDef | _:TopLevelModuleExportDef =>
      case topLevelMethodExport: TopLevelMethodExportDef =>
        topLevelMethodExports ::= topLevelMethodExport
      case topLevelFieldExport: TopLevelFieldExportDef =>
        topLevelFieldExports ::= topLevelFieldExport
    }

    if (topLevelMethodExports.nonEmpty || topLevelFieldExports.nonEmpty) {
      Some(new GenInfoTraverser().generateTopLevelExportsInfo(enclosingClass,
          topLevelMethodExports, topLevelFieldExports))
    } else {
      None
    }
  }

  private final class GenInfoTraverser extends Traverser {
    private val builder = new ReachabilityInfoBuilder

    def generateMethodInfo(methodDef: MethodDef): MethodInfo = {
      val methodName = methodDef.methodName
      methodName.paramTypeRefs.foreach(builder.maybeAddReferencedClass)
      builder.maybeAddReferencedClass(methodName.resultTypeRef)

      methodDef.body.foreach(traverse)

      val reachabilityInfo = builder.result()

      MethodInfo(
          methodName,
          methodDef.flags.namespace,
          methodDef.body.isEmpty,
          reachabilityInfo
      )
    }

    def generateJSMethodInfo(methodDef: JSMethodDef): ReachabilityInfo = {
      traverse(methodDef.name)
      traverse(methodDef.body)

      builder.result()
    }

    def generateJSPropertyInfo(propertyDef: JSPropertyDef): ReachabilityInfo = {
      traverse(propertyDef.name)
      propertyDef.getterBody.foreach(traverse)
      propertyDef.setterArgAndBody foreach { case (_, body) =>
        traverse(body)
      }

      builder.result()
    }

    def generateTopLevelExportsInfo(enclosingClass: ClassName,
        topLevelMethodExports: List[TopLevelMethodExportDef],
        topLevelFieldExports: List[TopLevelFieldExportDef]): ReachabilityInfo = {

      for (topLevelMethodExport <- topLevelMethodExports) {
        assert(topLevelMethodExport.methodDef.name.isInstanceOf[StringLiteral])
        traverse(topLevelMethodExport.methodDef.body)
      }

      for (topLevelFieldExport <- topLevelFieldExports) {
        val field = topLevelFieldExport.field.name
        builder.addStaticFieldRead(enclosingClass, field)
        builder.addStaticFieldWritten(enclosingClass, field)
      }

      builder.result()
    }

    override def traverse(tree: Tree): Unit = {
      builder.maybeAddReferencedClass(tree.tpe)

      tree match {
        /* Do not call super.traverse() so that the field is not also marked as
         * read.
         */
        case Assign(SelectStatic(className, field), rhs) =>
          builder.addStaticFieldWritten(className, field.name)
          traverse(rhs)

        // In all other cases, we'll have to call super.traverse()
        case _ =>
          tree match {
            case New(className, ctor, _) =>
              builder.addInstantiatedClass(className, ctor.name)

            case Select(_, className, _) =>
              builder.addReferencedClass(className)
            case SelectStatic(className, field) =>
              builder.addStaticFieldRead(className, field.name)

            case Apply(flags, receiver, method, _) =>
              builder.addMethodCalled(receiver.tpe, method.name)
            case ApplyStatically(flags, _, className, method, _) =>
              val namespace = MemberNamespace.forNonStaticCall(flags)
              builder.addMethodCalledStatically(className,
                  NamespacedMethodName(namespace, method.name))
            case ApplyStatic(flags, className, method, _) =>
              val namespace = MemberNamespace.forStaticCall(flags)
              builder.addMethodCalledStatically(className,
                  NamespacedMethodName(namespace, method.name))

            case LoadModule(className) =>
              builder.addAccessedModule(className)

            case IsInstanceOf(_, testType) =>
              builder.maybeAddUsedInstanceTest(testType)
            case AsInstanceOf(_, tpe) =>
              builder.maybeAddUsedInstanceTest(tpe)

            case BinaryOp(op, _, rhs) =>
              import BinaryOp._

              def addArithmeticException(): Unit = {
                builder.addInstantiatedClass(ArithmeticExceptionClass,
                    StringArgConstructorName)
              }

              op match {
                case Int_/ | Int_% =>
                  rhs match {
                    case IntLiteral(r) if r != 0 =>
                    case _                       => addArithmeticException()
                  }
                case Long_/ | Long_% =>
                  rhs match {
                    case LongLiteral(r) if r != 0L =>
                    case _                         => addArithmeticException()
                  }
                case _ =>
                  // do nothing
              }

            case NewArray(typeRef, _) =>
              builder.maybeAddAccessedClassData(typeRef)
            case ArrayValue(typeRef, _) =>
              builder.maybeAddAccessedClassData(typeRef)
            case ClassOf(cls) =>
              builder.maybeAddAccessedClassData(cls)

            case JSPrivateSelect(qualifier, className, field) =>
              builder.addPrivateJSFieldUsed(className, field.name)

            case LoadJSConstructor(className) =>
              builder.addInstantiatedClass(className)

            case LoadJSModule(className) =>
              builder.addAccessedModule(className)

            case CreateJSClass(className, _) =>
              builder.addInstantiatedClass(className)

            case Transient(CallHelper(_, args)) =>
              // This should only happen when called from the Refiner
              args.foreach(traverse)

            case VarDef(_, _, vtpe, _, _) =>
              builder.maybeAddReferencedClass(vtpe)

            case _ =>
          }

          super.traverse(tree)
      }
    }
  }

}
