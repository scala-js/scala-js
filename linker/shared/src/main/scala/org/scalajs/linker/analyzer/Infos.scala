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
import org.scalajs.ir.Version

import org.scalajs.linker.backend.emitter.Transients._
import org.scalajs.linker.standard.LinkedTopLevelExport
import org.scalajs.linker.standard.ModuleSet.ModuleID
import org.scalajs.linker.standard.LinkTimeProperties

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

  final class ClassInfo(
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
      val methods: Array[Map[MethodName, MethodInfo]],
      val jsNativeMembers: Map[MethodName, JSNativeLoadSpec],
      val jsMethodProps: List[ReachabilityInfo],
      val topLevelExports: List[TopLevelExportInfo]
  ) {
    override def toString(): String = className.nameString
  }

  /* MethodInfo should contain, not be a ReachbilityInfo
   *
   * However, since this class is retained over multiple linker runs in the
   * cache, the shallow size of the object shows up in memory performance
   * profiles. Therefore, we (ab)use inheritance to lower the memory overhead.
   */
  final class MethodInfo private (
    val isAbstract: Boolean,
    version: Version,
    byClass: Array[ReachabilityInfoInClass],
    globalFlags: ReachabilityInfo.Flags
  ) extends ReachabilityInfo(version, byClass, globalFlags)

  object MethodInfo {
    def apply(isAbstract: Boolean, reachabilityInfo: ReachabilityInfo): MethodInfo = {
      import reachabilityInfo._
      new MethodInfo(isAbstract, version, byClass, globalFlags)
    }
  }

  final class TopLevelExportInfo private[Infos] (
      val reachability: ReachabilityInfo,
      val moduleID: ModuleID,
      val exportName: String
  )

  sealed class ReachabilityInfo private[Infos] (
    /* The version field does not belong here conceptually.
     * However, it helps the InfoLoader re-use previous infos without
     * additional data held in memory.
     * This reduces the memory we need to cache infos between incremental runs.
     */
    val version: Version,
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
      /* We use a single field for all members to reduce memory consumption:
       * Typically, there are very few members reached in a single
       * ReachabilityInfoInClass, so the overhead of having a field per type
       * becomes significant in terms of memory usage.
       */
      val memberInfos: Array[MemberReachabilityInfo], // nullable!
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
    final val FlagDynamicallyReferenced = 1 << 5
  }

  sealed trait MemberReachabilityInfo

  final case class FieldReachable private[Infos] (
    val fieldName: FieldName,
    val read: Boolean = false,
    val written: Boolean = false
  ) extends MemberReachabilityInfo

  final case class StaticFieldReachable private[Infos] (
    val fieldName: FieldName,
    val read: Boolean = false,
    val written: Boolean = false
  ) extends MemberReachabilityInfo

  final case class MethodReachable private[Infos] (
    val methodName: MethodName
  ) extends MemberReachabilityInfo

  final case class MethodStaticallyReachable private[Infos] (
    val namespace: MemberNamespace,
    val methodName: MethodName
  ) extends MemberReachabilityInfo

  object MethodStaticallyReachable {
    private[Infos] def apply(m: NamespacedMethodName): MethodStaticallyReachable =
      MethodStaticallyReachable(m.namespace, m.methodName)
  }

  final case class JSNativeMemberReachable private[Infos] (
    val methodName: MethodName
  ) extends MemberReachabilityInfo

  def genReferencedFieldClasses(fields: List[AnyFieldDef]): Map[FieldName, ClassName] = {
    val builder = Map.newBuilder[FieldName, ClassName]

    fields.foreach {
      case FieldDef(flags, FieldIdent(name), _, ftpe) =>
        if (!flags.namespace.isStatic) {
          ftpe match {
            case ClassType(cls) =>
              builder += name -> cls
            case ArrayType(ArrayTypeRef(ClassRef(cls), _)) =>
              builder += name -> cls
            case _ =>
          }
        }
      case _: JSFieldDef =>
        // Nothing to do.
    }

    builder.result()
  }

  final class ReachabilityInfoBuilder(version: Version) {
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
      new ReachabilityInfo(version, byClass.valuesIterator.map(_.result()).toArray, flags)
  }

  final class ReachabilityInfoInClassBuilder(val className: ClassName) {
    private val fieldsUsed = mutable.Map.empty[FieldName, FieldReachable]
    private val staticFieldsUsed = mutable.Map.empty[FieldName, StaticFieldReachable]
    private val methodsCalled = mutable.Set.empty[MethodName]
    private val methodsCalledStatically = mutable.Set.empty[NamespacedMethodName]
    private val jsNativeMembersUsed = mutable.Set.empty[MethodName]
    private var flags: ReachabilityInfoInClass.Flags = 0

    def addFieldRead(field: FieldName): this.type = {
      fieldsUsed(field) = fieldsUsed
        .getOrElse(field, FieldReachable(field))
        .copy(read = true)
      this
    }

    def addFieldWritten(field: FieldName): this.type = {
      fieldsUsed(field) = fieldsUsed
        .getOrElse(field, FieldReachable(field))
        .copy(written = true)
      this
    }

    def addStaticFieldRead(field: FieldName): this.type = {
      staticFieldsUsed(field) = staticFieldsUsed
        .getOrElse(field, StaticFieldReachable(field))
        .copy(read = true)
      setStaticallyReferenced()
      this
    }

    def addStaticFieldWritten(field: FieldName): this.type = {
      staticFieldsUsed(field) = staticFieldsUsed
        .getOrElse(field, StaticFieldReachable(field))
        .copy(written = true)
      setStaticallyReferenced()
      this
    }

    def addMethodCalled(method: MethodName): this.type = {
      methodsCalled += method
      // Do not call setStaticallyReferenced: We call these methods on the object.
      this
    }

    def addMethodCalledStatically(method: NamespacedMethodName): this.type = {
      methodsCalledStatically += method
      setStaticallyReferenced()
      this
    }

    def addMethodCalledDynamicImport(method: NamespacedMethodName): this.type = {
      // In terms of reachability, a dynamic import call is just a static call.
      methodsCalledStatically += method
      setFlag(ReachabilityInfoInClass.FlagDynamicallyReferenced)
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
      val memberInfos: Array[MemberReachabilityInfo] = (
          fieldsUsed.valuesIterator ++
          staticFieldsUsed.valuesIterator ++
          methodsCalled.iterator.map(MethodReachable(_)) ++
          methodsCalledStatically.iterator.map(MethodStaticallyReachable(_)) ++
          jsNativeMembersUsed.iterator.map(JSNativeMemberReachable(_))
      ).toArray

      val memberInfosOrNull =
        if (memberInfos.isEmpty) null
        else memberInfos

      new ReachabilityInfoInClass(className, memberInfosOrNull, flags)
    }
  }

  /** Generates the [[MethodInfo]] of a
   *  [[org.scalajs.ir.Trees.MethodDef Trees.MethodDef]].
   */
  def generateMethodInfo(methodDef: MethodDef, linkTimeProperties: LinkTimeProperties): MethodInfo =
    new GenInfoTraverser(methodDef.version, linkTimeProperties).generateMethodInfo(methodDef)

  /** Generates the [[ReachabilityInfo]] of a
   *  [[org.scalajs.ir.Trees.JSConstructorDef Trees.JSConstructorDef]].
   */
  def generateJSConstructorInfo(ctorDef: JSConstructorDef, linkTimeProperties: LinkTimeProperties): ReachabilityInfo =
    new GenInfoTraverser(ctorDef.version, linkTimeProperties).generateJSConstructorInfo(ctorDef)

  /** Generates the [[ReachabilityInfo]] of a
   *  [[org.scalajs.ir.Trees.JSMethodDef Trees.JSMethodDef]].
   */
  def generateJSMethodInfo(methodDef: JSMethodDef, linkTimeProperties: LinkTimeProperties): ReachabilityInfo =
    new GenInfoTraverser(methodDef.version, linkTimeProperties).generateJSMethodInfo(methodDef)

  /** Generates the [[ReachabilityInfo]] of a
   *  [[org.scalajs.ir.Trees.JSPropertyDef Trees.JSPropertyDef]].
   */
  def generateJSPropertyInfo(propertyDef: JSPropertyDef, linkTimeProperties: LinkTimeProperties): ReachabilityInfo =
    new GenInfoTraverser(propertyDef.version, linkTimeProperties).generateJSPropertyInfo(propertyDef)

  def generateJSMethodPropDefInfo(member: JSMethodPropDef, linkTimeProperties: LinkTimeProperties): ReachabilityInfo =
    member match {
      case methodDef: JSMethodDef     => generateJSMethodInfo(methodDef, linkTimeProperties)
      case propertyDef: JSPropertyDef => generateJSPropertyInfo(propertyDef, linkTimeProperties)
    }

  /** Generates the [[MethodInfo]] for the top-level exports. */
  def generateTopLevelExportInfo(enclosingClass: ClassName,
      topLevelExportDef: TopLevelExportDef, linkTimeProperties: LinkTimeProperties): TopLevelExportInfo = {
    val info = new GenInfoTraverser(Version.Unversioned, linkTimeProperties)
        .generateTopLevelExportInfo(enclosingClass, topLevelExportDef)
    new TopLevelExportInfo(info,
        ModuleID(topLevelExportDef.moduleID),
        topLevelExportDef.topLevelExportName)
  }

  private final class GenInfoTraverser(version: Version, linkTimeProperties: LinkTimeProperties) extends Traverser {
    private val builder = new ReachabilityInfoBuilder(version)

    def generateMethodInfo(methodDef: MethodDef): MethodInfo = {
      val methodName = methodDef.methodName
      methodName.paramTypeRefs.foreach(builder.maybeAddReferencedClass)
      builder.maybeAddReferencedClass(methodName.resultTypeRef)

      methodDef.body.foreach(traverse)

      val reachabilityInfo = builder.result()

      MethodInfo(methodDef.body.isEmpty, reachabilityInfo)
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

        case LinkTimeIf(cond, thenp, elsep) =>
          if (linkTimeProperties.evaluateLinkTimeTree(cond))
            traverse(thenp)
          else
            traverse(elsep)

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
