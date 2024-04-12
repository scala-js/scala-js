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
import org.scalajs.linker.standard.LinkedTopLevelExport
import org.scalajs.linker.standard.ModuleSet.ModuleID

object Infos {

  private val StringArgConstructorName =
    MethodName.constructor(List(ClassRef(BoxedStringClass)))

  private val cloneMethodName = MethodName("clone", Nil, ClassRef(ObjectClass))

  /* Elements of WrapAsThrowable and UnwrapFromThrowable used by the Emitter
   * In theory, these should be an implementation detail of the Emitter, and
   * requested through `symbolRequirements`. However, doing so would mean that
   * we would never be able to dead-code-eliminate JavaScriptException, which
   * would be annoying.
   */
  private val JavaScriptExceptionClass = ClassName("scala.scalajs.js.JavaScriptException")
  private val AnyArgConstructorName = MethodName.constructor(List(ClassRef(ObjectClass)))

  final case class NamespacedMethodName(
      namespace: MemberNamespace, methodName: MethodName)

  final class ClassInfo private[Infos] (
      val className: ClassName,
      val kind: ClassKind,
      val superClass: Option[ClassName], // always None for interfaces
      val interfaces: List[ClassName], // direct parent interfaces only
      val jsNativeLoadSpec: Option[JSNativeLoadSpec],
      /* Referenced classes of non-static fields.
       *
       * Note that the classes referenced by static fields are reached
       * implicitly by the call-sites that read or write the field: the
       * SelectStatic expression has the same type as the field.
       *
       * Further, note that some existing fields might not appear in this map:
       * This happens when they have non-class type (e.g. Int)
       */
      val referencedFieldClasses: Map[FieldName, ClassName],
      val methods: List[MethodInfo],
      val jsNativeMembers: Map[MethodName, JSNativeLoadSpec],
      val jsMethodProps: List[ReachabilityInfo],
      val topLevelExports: List[TopLevelExportInfo]
  ) {
    override def toString(): String = className.nameString
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

  final class TopLevelExportInfo private[Infos] (
      val reachability: ReachabilityInfo,
      val moduleID: ModuleID,
      val exportName: String
  )

  final class ReachabilityInfo private[Infos] (
      val byClass: Array[ReachabilityInfoInClass],
      val globalFlags: ReachabilityInfo.Flags
  )

  object ReachabilityInfo {
    type Flags = Int

    final val FlagAccessedClassClass = 1 << 0
    final val FlagAccessedNewTarget = 1 << 1
    final val FlagAccessedImportMeta = 1 << 2
    final val FlagUsedExponentOperator = 1 << 3
  }

  /** Things from a given class that are reached by one method. */
  final class ReachabilityInfoInClass private[Infos] (
      val className: ClassName,
      val fieldsRead: List[FieldName],
      val fieldsWritten: List[FieldName],
      val staticFieldsRead: List[FieldName],
      val staticFieldsWritten: List[FieldName],
      val methodsCalled: List[MethodName],
      val methodsCalledStatically: List[NamespacedMethodName],
      val methodsCalledDynamicImport: List[NamespacedMethodName],
      val jsNativeMembersUsed: List[MethodName],
      val flags: ReachabilityInfoInClass.Flags
  )

  object ReachabilityInfoInClass {
    type Flags = Int

    /** For a Scala class, it is instantiated with a `New`; for a JS class,
     *  its constructor is accessed with a `JSLoadConstructor`.
     */
    final val FlagInstantiated = 1 << 0

    final val FlagModuleAccessed = 1 << 1
    final val FlagInstanceTestsUsed = 1 << 2
    final val FlagClassDataAccessed = 1 << 3
    final val FlagStaticallyReferenced = 1 << 4
  }

  final class ClassInfoBuilder(
      private val className: ClassName,
      private val kind: ClassKind,
      private val superClass: Option[ClassName],
      private val interfaces: List[ClassName],
      private val jsNativeLoadSpec: Option[JSNativeLoadSpec]
  ) {
    private val referencedFieldClasses = mutable.Map.empty[FieldName, ClassName]
    private val methods = mutable.ListBuffer.empty[MethodInfo]
    private val jsNativeMembers = mutable.Map.empty[MethodName, JSNativeLoadSpec]
    private val jsMethodProps = mutable.ListBuffer.empty[ReachabilityInfo]
    private val topLevelExports = mutable.ListBuffer.empty[TopLevelExportInfo]

    def maybeAddReferencedFieldClass(name: FieldName, tpe: Type): this.type = {
      tpe match {
        case ClassType(cls) =>
          referencedFieldClasses.put(name, cls)
        case ArrayType(ArrayTypeRef(ClassRef(cls), _)) =>
          referencedFieldClasses.put(name, cls)
        case _ =>
      }

      this
    }

    def addMethod(methodInfo: MethodInfo): this.type = {
      methods += methodInfo
      this
    }

    def addJSNativeMember(member: JSNativeMemberDef): this.type = {
      jsNativeMembers.put(member.name.name, member.jsNativeLoadSpec)
      this
    }

    def addExportedMember(reachabilityInfo: ReachabilityInfo): this.type = {
      jsMethodProps += reachabilityInfo
      this
    }

    def addTopLevelExport(topLevelExportInfo: TopLevelExportInfo): this.type = {
      topLevelExports += topLevelExportInfo
      this
    }

    def result(): ClassInfo = {
      new ClassInfo(className, kind, superClass,
          interfaces, jsNativeLoadSpec, referencedFieldClasses.toMap,
          methods.toList, jsNativeMembers.toMap, jsMethodProps.toList,
          topLevelExports.toList)
    }
  }

  final class ReachabilityInfoBuilder {
    private val byClass = mutable.Map.empty[ClassName, ReachabilityInfoInClassBuilder]
    private var flags: ReachabilityInfo.Flags = 0

    private def forClass(cls: ClassName): ReachabilityInfoInClassBuilder =
      byClass.getOrElseUpdate(cls, new ReachabilityInfoInClassBuilder(cls))

    def addFieldRead(field: FieldName): this.type = {
      forClass(field.className).addFieldRead(field)
      this
    }

    def addFieldWritten(field: FieldName): this.type = {
      forClass(field.className).addFieldWritten(field)
      this
    }

    def addStaticFieldRead(field: FieldName): this.type = {
      forClass(field.className).addStaticFieldRead(field)
      this
    }

    def addStaticFieldWritten(field: FieldName): this.type = {
      forClass(field.className).addStaticFieldWritten(field)
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
           * so we need to cheat here. Since the Array[T] classes do not define
           * any method themselves--they are all inherited from j.l.Object--,
           * we can model their reachability by calling them statically in the
           * Object class.
           */
          addMethodCalledStatically(ObjectClass,
              NamespacedMethodName(MemberNamespace.Public, method))

        case NullType | NothingType =>
          // Nothing to do

        case NoType | RecordType(_) =>
          throw new IllegalArgumentException(
              s"Illegal receiver type: $receiverTpe")
      }

      this
    }

    def addMethodCalled(cls: ClassName, method: MethodName): this.type = {
      forClass(cls).addMethodCalled(method)
      this
    }

    def addMethodCalledStatically(cls: ClassName,
        method: NamespacedMethodName): this.type = {
      forClass(cls).addMethodCalledStatically(method)
      this
    }

    def addMethodCalledDynamicImport(cls: ClassName,
        method: NamespacedMethodName): this.type = {
      forClass(cls).addMethodCalledDynamicImport(method)
      this
    }

    def addJSNativeMemberUsed(cls: ClassName, member: MethodName): this.type = {
      forClass(cls).addJSNativeMemberUsed(member)
      this
    }

    def addInstantiatedClass(cls: ClassName): this.type = {
      forClass(cls).setInstantiated()
      this
    }

    def addInstantiatedClass(cls: ClassName, ctor: MethodName): this.type = {
      forClass(cls).setInstantiated().addMethodCalledStatically(
          NamespacedMethodName(MemberNamespace.Constructor, ctor))
      this
    }

    def addAccessedModule(cls: ClassName): this.type = {
      forClass(cls).setModuleAccessed()
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
      forClass(cls).setInstanceTestsUsed()
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
      forClass(cls).setClassDataAccessed()
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
      /* We only need the class to appear in `byClass` so that the Analyzer
       * knows to perform `lookupClass` for it. But then nothing further needs
       * to happen.
       */
      forClass(cls)
      this
    }

    def addStaticallyReferencedClass(cls: ClassName): this.type = {
      forClass(cls).setStaticallyReferenced()
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

    private def setFlag(flag: ReachabilityInfo.Flags): this.type = {
      flags |= flag
      this
    }

    def addAccessedClassClass(): this.type =
      setFlag(ReachabilityInfo.FlagAccessedClassClass)

    def addAccessNewTarget(): this.type =
      setFlag(ReachabilityInfo.FlagAccessedNewTarget)

    def addAccessImportMeta(): this.type =
      setFlag(ReachabilityInfo.FlagAccessedImportMeta)

    def addUsedExponentOperator(): this.type =
      setFlag(ReachabilityInfo.FlagUsedExponentOperator)

    def result(): ReachabilityInfo =
      new ReachabilityInfo(byClass.valuesIterator.map(_.result()).toArray, flags)
  }

  final class ReachabilityInfoInClassBuilder(val className: ClassName) {
    private val fieldsRead = mutable.Set.empty[FieldName]
    private val fieldsWritten = mutable.Set.empty[FieldName]
    private val staticFieldsRead = mutable.Set.empty[FieldName]
    private val staticFieldsWritten = mutable.Set.empty[FieldName]
    private val methodsCalled = mutable.Set.empty[MethodName]
    private val methodsCalledStatically = mutable.Set.empty[NamespacedMethodName]
    private val methodsCalledDynamicImport = mutable.Set.empty[NamespacedMethodName]
    private val jsNativeMembersUsed = mutable.Set.empty[MethodName]
    private var flags: ReachabilityInfoInClass.Flags = 0

    def addFieldRead(field: FieldName): this.type = {
      fieldsRead += field
      this
    }

    def addFieldWritten(field: FieldName): this.type = {
      fieldsWritten += field
      this
    }

    def addStaticFieldRead(field: FieldName): this.type = {
      staticFieldsRead += field
      this
    }

    def addStaticFieldWritten(field: FieldName): this.type = {
      staticFieldsWritten += field
      this
    }

    def addMethodCalled(method: MethodName): this.type = {
      methodsCalled += method
      this
    }

    def addMethodCalledStatically(method: NamespacedMethodName): this.type = {
      methodsCalledStatically += method
      this
    }

    def addMethodCalledDynamicImport(method: NamespacedMethodName): this.type = {
      methodsCalledDynamicImport += method
      this
    }

    def addJSNativeMemberUsed(member: MethodName): this.type = {
      jsNativeMembersUsed += member
      this
    }

    private def setFlag(flag: ReachabilityInfoInClass.Flags): this.type = {
      flags |= flag
      this
    }

    def setInstantiated(): this.type =
      setFlag(ReachabilityInfoInClass.FlagInstantiated)

    def setModuleAccessed(): this.type =
      setFlag(ReachabilityInfoInClass.FlagModuleAccessed)

    def setInstanceTestsUsed(): this.type =
      setFlag(ReachabilityInfoInClass.FlagInstanceTestsUsed)

    def setClassDataAccessed(): this.type =
      setFlag(ReachabilityInfoInClass.FlagClassDataAccessed)

    def setStaticallyReferenced(): this.type =
      setFlag(ReachabilityInfoInClass.FlagStaticallyReferenced)

    def result(): ReachabilityInfoInClass = {
      new ReachabilityInfoInClass(
          className,
          fieldsRead = toLikelyEmptyList(fieldsRead),
          fieldsWritten = toLikelyEmptyList(fieldsWritten),
          staticFieldsRead = toLikelyEmptyList(staticFieldsRead),
          staticFieldsWritten = toLikelyEmptyList(staticFieldsWritten),
          methodsCalled = toLikelyEmptyList(methodsCalled),
          methodsCalledStatically = toLikelyEmptyList(methodsCalledStatically),
          methodsCalledDynamicImport = toLikelyEmptyList(methodsCalledDynamicImport),
          jsNativeMembersUsed = toLikelyEmptyList(jsNativeMembersUsed),
          flags = flags
      )
    }

    private def toLikelyEmptyList[A](set: mutable.Set[A]): List[A] =
      if (set.isEmpty) Nil
      else set.toList
  }

  /** Generates the [[MethodInfo]] of a
   *  [[org.scalajs.ir.Trees.MethodDef Trees.MethodDef]].
   */
  def generateMethodInfo(methodDef: MethodDef): MethodInfo =
    new GenInfoTraverser().generateMethodInfo(methodDef)

  /** Generates the [[ReachabilityInfo]] of a
   *  [[org.scalajs.ir.Trees.JSConstructorDef Trees.JSConstructorDef]].
   */
  def generateJSConstructorInfo(ctorDef: JSConstructorDef): ReachabilityInfo =
    new GenInfoTraverser().generateJSConstructorInfo(ctorDef)

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
  def generateTopLevelExportInfo(enclosingClass: ClassName,
      topLevelExportDef: TopLevelExportDef): TopLevelExportInfo = {
    val info = new GenInfoTraverser().generateTopLevelExportInfo(enclosingClass,
        topLevelExportDef)
    new TopLevelExportInfo(info,
        ModuleID(topLevelExportDef.moduleID),
        topLevelExportDef.topLevelExportName)
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

    def generateJSConstructorInfo(ctorDef: JSConstructorDef): ReachabilityInfo = {
      ctorDef.body.allStats.foreach(traverse(_))

      builder.result()
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

    def generateTopLevelExportInfo(enclosingClass: ClassName,
        topLevelExportDef: TopLevelExportDef): ReachabilityInfo = {
      topLevelExportDef match {
        case _:TopLevelJSClassExportDef =>
          builder.addInstantiatedClass(enclosingClass)

        case _:TopLevelModuleExportDef =>
          builder.addAccessedModule(enclosingClass)

        case topLevelMethodExport: TopLevelMethodExportDef =>
          assert(topLevelMethodExport.methodDef.name.isInstanceOf[StringLiteral])
          traverse(topLevelMethodExport.methodDef.body)

        case topLevelFieldExport: TopLevelFieldExportDef =>
          val field = topLevelFieldExport.field.name
          builder.addStaticFieldRead(field)
          builder.addStaticFieldWritten(field)
      }

      builder.result()
    }

    override def traverse(tree: Tree): Unit = {
      builder.maybeAddReferencedClass(tree.tpe)

      tree match {
        /* Do not call super.traverse() so that fields are not also marked as
         * read.
         */
        case Assign(lhs, rhs) =>
          lhs match {
            case Select(qualifier, field) =>
              builder.addFieldWritten(field.name)
              traverse(qualifier)
            case SelectStatic(field) =>
              builder.addStaticFieldWritten(field.name)
            case JSPrivateSelect(qualifier, field) =>
              builder.addStaticallyReferencedClass(field.name.className) // for the private name of the field
              builder.addFieldWritten(field.name)
              traverse(qualifier)
            case _ =>
              traverse(lhs)
          }
          traverse(rhs)

        // In all other cases, we'll have to call super.traverse()
        case _ =>
          tree match {
            case New(className, ctor, _) =>
              builder.addInstantiatedClass(className, ctor.name)

            case Select(_, field) =>
              builder.addFieldRead(field.name)
            case SelectStatic(field) =>
              builder.addStaticFieldRead(field.name)
            case SelectJSNativeMember(className, member) =>
              builder.addJSNativeMemberUsed(className, member.name)

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
            case ApplyDynamicImport(flags, className, method, _) =>
              val namespace = MemberNamespace.forStaticCall(flags)
              builder.addMethodCalledDynamicImport(className,
                  NamespacedMethodName(namespace, method.name))

            case LoadModule(className) =>
              builder.addAccessedModule(className)

            case IsInstanceOf(_, testType) =>
              builder.maybeAddUsedInstanceTest(testType)

            case AsInstanceOf(_, tpe) =>
              /* In theory, we'd need to reach ClassCastException
               * here (conditional on the semantics) by IR spec.
               * However, since the exact *constructor* is not specified, this
               * makes little sense.
               * Instead, the Emitter simply requests the exception in its
               * symbol requirements.
               */

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

            case ArraySelect(_, _) =>
              /* In theory, we'd need to reach ArrayIndexOutOfBoundsException
               * here (conditional on the semantics) by IR spec.
               * However, since the exact *constructor* is not specified, this
               * makes little sense.
               * Instead, the Emitter simply requests the exception in its
               * symbol requirements.
               */

            case ClassOf(cls) =>
              builder.maybeAddAccessedClassData(cls)
              builder.addAccessedClassClass()

            case GetClass(_) =>
              builder.addAccessedClassClass()

            case WrapAsThrowable(_) =>
              builder.addUsedInstanceTest(ThrowableClass)
              builder.addInstantiatedClass(JavaScriptExceptionClass, AnyArgConstructorName)

            case UnwrapFromThrowable(_) =>
              builder.addUsedInstanceTest(JavaScriptExceptionClass)

            case JSPrivateSelect(_, field) =>
              builder.addStaticallyReferencedClass(field.name.className) // for the private name of the field
              builder.addFieldRead(field.name)

            case JSNewTarget() =>
              builder.addAccessNewTarget()

            case JSImportMeta() =>
              builder.addAccessImportMeta()

            case JSBinaryOp(JSBinaryOp.**, _, _) =>
              builder.addUsedExponentOperator()

            case LoadJSConstructor(className) =>
              builder.addInstantiatedClass(className)

            case LoadJSModule(className) =>
              builder.addAccessedModule(className)

            case CreateJSClass(className, _) =>
              builder.addInstantiatedClass(className)

            case VarDef(_, _, vtpe, _, _) =>
              builder.maybeAddReferencedClass(vtpe)

            case _ =>
          }

          super.traverse(tree)
      }
    }
  }

}
