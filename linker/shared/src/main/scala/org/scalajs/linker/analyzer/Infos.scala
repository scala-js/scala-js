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
import org.scalajs.ir.Definitions._
import org.scalajs.ir.Traversers._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.linker.backend.emitter.Transients._

object Infos {

  final case class NamespacedEncodedName(
      namespace: MemberNamespace, encodedName: String)

  final class ClassInfo private (
      val encodedName: String,
      val isExported: Boolean,
      val kind: ClassKind,
      val superClass: Option[String], // always None for interfaces
      val interfaces: List[String], // direct parent interfaces only
      val referencedFieldClasses: List[String],
      val methods: List[MethodInfo],
      val exportedMembers: List[ReachabilityInfo],
      val topLevelExportedMembers: List[ReachabilityInfo],
      val topLevelExportNames: List[String]
  ) {
    override def toString(): String = encodedName
  }

  object ClassInfo {
    def apply(
        encodedName: String,
        isExported: Boolean,
        kind: ClassKind,
        superClass: Option[String],
        interfaces: List[String],
        referencedFieldClasses: List[String],
        methods: List[MethodInfo],
        exportedMembers: List[ReachabilityInfo],
        topLevelExportedMembers: List[ReachabilityInfo],
        topLevelExportNames: List[String]): ClassInfo = {
      new ClassInfo(encodedName, isExported, kind, superClass,
          interfaces, referencedFieldClasses, methods, exportedMembers,
          topLevelExportedMembers, topLevelExportNames)
    }
  }

  final class MethodInfo private (
      val encodedName: String,
      val namespace: MemberNamespace,
      val isAbstract: Boolean,
      val reachabilityInfo: ReachabilityInfo
  ) {
    override def toString(): String = encodedName
  }

  object MethodInfo {
    def apply(
        encodedName: String,
        namespace: MemberNamespace,
        isAbstract: Boolean,
        reachabilityInfo: ReachabilityInfo): MethodInfo = {
      new MethodInfo(encodedName, namespace, isAbstract, reachabilityInfo)
    }
  }

  final class ReachabilityInfo private (
      val privateJSFieldsUsed: Map[String, List[String]],
      val staticFieldsRead: Map[String, List[String]],
      val staticFieldsWritten: Map[String, List[String]],
      val methodsCalled: Map[String, List[String]],
      val methodsCalledStatically: Map[String, List[NamespacedEncodedName]],
      /** For a Scala class, it is instantiated with a `New`; for a JS class,
       *  its constructor is accessed with a `JSLoadConstructor`.
       */
      val instantiatedClasses: List[String],
      val accessedModules: List[String],
      val usedInstanceTests: List[String],
      val accessedClassData: List[String],
      val referencedClasses: List[String]
  )

  object ReachabilityInfo {
    val Empty: ReachabilityInfo = {
      ReachabilityInfo(
          Map.empty, Map.empty, Map.empty, Map.empty, Map.empty,
          Nil, Nil, Nil, Nil, Nil)
    }

    def apply(
        privateJSFieldsUsed: Map[String, List[String]],
        staticFieldsRead: Map[String, List[String]],
        staticFieldsWritten: Map[String, List[String]],
        methodsCalled: Map[String, List[String]],
        methodsCalledStatically: Map[String, List[NamespacedEncodedName]],
        instantiatedClasses: List[String],
        accessedModules: List[String],
        usedInstanceTests: List[String],
        accessedClassData: List[String],
        referencedClasses: List[String]): ReachabilityInfo = {
      new ReachabilityInfo(privateJSFieldsUsed, staticFieldsRead,
          staticFieldsWritten, methodsCalled, methodsCalledStatically,
          instantiatedClasses, accessedModules, usedInstanceTests,
          accessedClassData, referencedClasses)
    }
  }

  final class ClassInfoBuilder {
    private var encodedName: String = ""
    private var kind: ClassKind = ClassKind.Class
    private var isExported: Boolean = false
    private var superClass: Option[String] = None
    private val interfaces = mutable.ListBuffer.empty[String]
    private val referencedFieldClasses = mutable.Set.empty[String]
    private val methods = mutable.ListBuffer.empty[MethodInfo]
    private val exportedMembers = mutable.ListBuffer.empty[ReachabilityInfo]
    private val topLevelExportedMembers = mutable.ListBuffer.empty[ReachabilityInfo]
    private var topLevelExportNames: List[String] = Nil

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

    def addInterfaces(interfaces: List[String]): this.type = {
      this.interfaces ++= interfaces
      this
    }

    def maybeAddReferencedFieldClass(tpe: Type): this.type = {
      tpe match {
        case ClassType(cls)                   => referencedFieldClasses += cls
        case ArrayType(ArrayTypeRef(base, _)) => referencedFieldClasses += base
        case _                                =>
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
      ClassInfo(encodedName, isExported, kind, superClass,
          interfaces.toList, referencedFieldClasses.toList, methods.toList,
          exportedMembers.toList, topLevelExportedMembers.toList,
          topLevelExportNames)
    }
  }

  final class ReachabilityInfoBuilder {
    private val privateJSFieldsUsed = mutable.Map.empty[String, mutable.Set[String]]
    private val staticFieldsRead = mutable.Map.empty[String, mutable.Set[String]]
    private val staticFieldsWritten = mutable.Map.empty[String, mutable.Set[String]]
    private val methodsCalled = mutable.Map.empty[String, mutable.Set[String]]
    private val methodsCalledStatically = mutable.Map.empty[String, mutable.Set[NamespacedEncodedName]]
    private val instantiatedClasses = mutable.Set.empty[String]
    private val accessedModules = mutable.Set.empty[String]
    private val usedInstanceTests = mutable.Set.empty[String]
    private val accessedClassData = mutable.Set.empty[String]
    private val referencedClasses = mutable.Set.empty[String]

    def addPrivateJSFieldUsed(cls: String, field: String): this.type = {
      privateJSFieldsUsed.getOrElseUpdate(cls, mutable.Set.empty) += field
      this
    }

    def addStaticFieldRead(cls: String, field: String): this.type = {
      staticFieldsRead.getOrElseUpdate(cls, mutable.Set.empty) += field
      this
    }

    def addStaticFieldWritten(cls: String, field: String): this.type = {
      staticFieldsWritten.getOrElseUpdate(cls, mutable.Set.empty) += field
      this
    }

    def addMethodCalled(receiverTpe: Type, method: String): this.type = {
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
           * In the Array[T] class family, only clone__O is defined and
           * overrides j.l.Object.clone__O. Since this method is implemented
           * in CoreJSLib and always kept, we can ignore it.
           * All other methods resolve to their definition in Object, so we
           * can model their reachability by calling them statically in the
           * Object class.
           */
          if (method != "clone__O") {
            addMethodCalledStatically(ObjectClass,
                NamespacedEncodedName(MemberNamespace.Public, method))
          }

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

    def addMethodCalledStatically(cls: String,
        method: NamespacedEncodedName): this.type = {
      methodsCalledStatically.getOrElseUpdate(cls, mutable.Set.empty) += method
      this
    }

    def addInstantiatedClass(cls: String): this.type = {
      instantiatedClasses += cls
      this
    }

    def addInstantiatedClass(cls: String, ctor: String): this.type = {
      addInstantiatedClass(cls).addMethodCalledStatically(cls,
          NamespacedEncodedName(MemberNamespace.Constructor, ctor))
    }

    def addAccessedModule(cls: String): this.type = {
      accessedModules += cls
      this
    }

    def maybeAddUsedInstanceTest(tpe: Type): this.type = {
      tpe match {
        case ClassType(className) =>
          addUsedInstanceTest(className)
        case ArrayType(ArrayTypeRef(baseClassName, _)) =>
          addUsedInstanceTest(baseClassName)
        case _ =>
      }
      this
    }

    def addUsedInstanceTest(cls: String): this.type = {
      usedInstanceTests += cls
      this
    }

    def addAccessedClassData(tpe: TypeRef): this.type =
      addAccessedClassData(baseNameOf(tpe))

    def addAccessedClassData(cls: String): this.type = {
      accessedClassData += cls
      this
    }

    def addReferencedClass(tpe: TypeRef): this.type =
      addReferencedClass(baseNameOf(tpe))

    def addReferencedClass(cls: String): this.type = {
      referencedClasses += cls
      this
    }

    def maybeAddReferencedClass(tpe: Type): this.type = tpe match {
      case ClassType(cls)     => addReferencedClass(cls)
      case ArrayType(typeRef) => addReferencedClass(typeRef)
      case _                  => this
    }

    private def baseNameOf(tpe: TypeRef): String = tpe match {
      case ClassRef(name)        => name
      case ArrayTypeRef(base, _) => base
    }

    def result(): ReachabilityInfo = {
      def toMapOfLists[A](
          m: mutable.Map[String, mutable.Set[A]]): Map[String, List[A]] = {
        m.map(kv => kv._1 -> kv._2.toList).toMap
      }

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
    val builder = new ClassInfoBuilder()
      .setEncodedName(classDef.name.name)
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
  def generateTopLevelExportsInfo(enclosingClass: String,
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
      val (_, params, result) = decodeMethodName(methodDef.encodedName)
      params.foreach(builder.addReferencedClass)
      result.foreach(builder.addReferencedClass)

      methodDef.body.foreach(traverse)

      val reachabilityInfo = builder.result()

      MethodInfo(
          methodDef.encodedName,
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

    def generateTopLevelExportsInfo(enclosingClass: String,
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
        case Assign(SelectStatic(ClassRef(cls), Ident(field, _)), rhs) =>
          builder.addStaticFieldWritten(cls, field)
          traverse(rhs)

        // In all other cases, we'll have to call super.traverse()
        case _ =>
          tree match {
            case New(ClassRef(cls), ctor, _) =>
              builder.addInstantiatedClass(cls, ctor.name)

            case Select(_, ClassRef(cls), _) =>
              builder.addReferencedClass(cls)
            case SelectStatic(ClassRef(cls), Ident(field, _)) =>
              builder.addStaticFieldRead(cls, field)

            case Apply(flags, receiver, Ident(method, _), _) =>
              builder.addMethodCalled(receiver.tpe, method)
            case ApplyStatically(flags, _, ClassRef(cls), method, _) =>
              val namespace = MemberNamespace.forNonStaticCall(flags)
              builder.addMethodCalledStatically(cls,
                  NamespacedEncodedName(namespace, method.name))
            case ApplyStatic(flags, ClassRef(cls), method, _) =>
              val namespace = MemberNamespace.forStaticCall(flags)
              builder.addMethodCalledStatically(cls,
                  NamespacedEncodedName(namespace, method.name))

            case LoadModule(ClassRef(cls)) =>
              builder.addAccessedModule(cls)

            case IsInstanceOf(_, testType) =>
              builder.maybeAddUsedInstanceTest(testType)
            case AsInstanceOf(_, tpe) =>
              builder.maybeAddUsedInstanceTest(tpe)

            case BinaryOp(op, _, rhs) =>
              import BinaryOp._

              def addArithmeticException(): Unit =
                builder.addInstantiatedClass("jl_ArithmeticException", "init___T")

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
              builder.addAccessedClassData(typeRef)
            case ArrayValue(typeRef, _) =>
              builder.addAccessedClassData(typeRef)
            case ClassOf(cls) =>
              builder.addAccessedClassData(cls)

            case JSPrivateSelect(qualifier, cls, field) =>
              builder.addPrivateJSFieldUsed(cls.className, field.name)

            case LoadJSConstructor(cls) =>
              builder.addInstantiatedClass(cls.className)

            case LoadJSModule(ClassRef(cls)) =>
              builder.addAccessedModule(cls)

            case CreateJSClass(cls, _) =>
              builder.addInstantiatedClass(cls.className)

            case Transient(CallHelper(_, args)) =>
              // This should only happen when called from the Refiner
              args.foreach(traverse)

            case VarDef(_, vtpe, _, _) =>
              builder.maybeAddReferencedClass(vtpe)

            case _ =>
          }

          super.traverse(tree)
      }
    }
  }

}
