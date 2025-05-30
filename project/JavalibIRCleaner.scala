package build

import org.scalajs.ir._
import org.scalajs.ir.ClassKind
import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._
import org.scalajs.ir.Version.Unversioned
import org.scalajs.ir.WellKnownNames._

import java.io._
import java.net.URI
import java.nio._
import java.nio.file._
import java.nio.file.attribute._
import java.util.stream.{Stream, StreamSupport}

import scala.collection.immutable.IndexedSeq
import scala.collection.mutable
import scala.collection.JavaConverters._

import sbt.{Logger, MessageOnlyException}

/** Postprocessor for the IR of the javalanglib, to remove all references to
 *  JS types in the Scala library, and ensure that there remains no other
 *  reference to the Scala library.
 *
 *  This ensures that the IR of the javalanglib is truly independent of Scala.
 *
 *  The main task is to *completely* erase all references to JS types to
 *  `j.l.Object`. This includes:
 *
 *  - Delete (or do not copy over) .sjsir files that define abstract and native
 *    JS types.
 *  - Erase references to JS types in method signatures and `TypeRef`s to
 *    `java.lang.Object`.
 *  - Eagerly dereference `LoadJSConstructor` and `LoadJSModule` by "inlining"
 *    the JS load spec of the mentioned class ref.
 *  - Replace calls to "intrinsic" methods of the Scala.js library by their
 *    meaning at call site.
 *
 *  Afterwards, we check that the IR does not contain any reference to classes
 *  under the `scala.*` package.
 */
final class JavalibIRCleaner(baseDirectoryURI: URI) {
  import JavalibIRCleaner._

  type JSTypes = Map[ClassName, Option[JSNativeLoadSpec]]

  def cleanIR(dependencyFiles: Seq[File], libFileMappings: Seq[(File, File)],
      logger: Logger): Set[File] = {

    val errorManager = new ErrorManager(logger)

    val libIRMappings = for {
      (input, output) <- libFileMappings
    } yield {
      (readIR(input), output)
    }

    val jsTypes = {
      val dependencyIR: Stream[ClassDef] = {
        dependencyFiles.asJava.stream().flatMap { file =>
          if (file.getName().endsWith(".jar"))
            readIRJar(file)
          else
            Stream.of[ClassDef](readIR(file))
        }
      }
      val libIR: Stream[ClassDef] = libIRMappings.asJava.stream().map(_._1)
      getJSTypes(Stream.concat(dependencyIR, libIR))
    }

    val resultBuilder = Set.newBuilder[File]

    for ((tree, output) <- libIRMappings) {
      import ClassKind._

      tree.kind match {
        case Class | ModuleClass | Interface | HijackedClass =>
          val cleanedTree = cleanTree(tree, jsTypes, errorManager)
          writeIRFile(output, cleanedTree)
          resultBuilder += output

        case AbstractJSType | NativeJSClass | NativeJSModuleClass =>
          // discard

        case JSClass | JSModuleClass =>
          errorManager.reportError(
              s"found non-native JS class ${tree.className.nameString}")(tree.pos)
      }
    }

    if (errorManager.hasErrors) {
      throw new MessageOnlyException(
          s"There were ${errorManager.errorCount} errors while " +
          "postprocessing the IR of the javalanglib. " +
          "The javalanglib must be written in a style that does not leak any " +
          "reference to the Scala library.")
    }

    resultBuilder.result()
  }

  private final class ErrorManager(logger: Logger) {
    private val seenErrors = mutable.Set.empty[String]
    private var _errorCount: Int = 0

    def reportError(msg: String)(implicit pos: Position): Unit = {
      val fileStr = baseDirectoryURI.relativize(pos.source).toString
      val fullMessage = s"$msg at $fileStr:${pos.line}:${pos.column}"
      if (seenErrors.add(fullMessage)) {
        logger.error(fullMessage)
        _errorCount += 1
      }
    }

    def hasErrors: Boolean = _errorCount != 0

    def errorCount: Int = _errorCount
  }

  private def readIR(file: File): ClassDef =
    readIR(file.toPath())

  private def readIR(path: Path): ClassDef = {
    val bytes = Files.readAllBytes(path)
    val buffer = ByteBuffer.wrap(bytes)
    Serializers.deserialize(buffer)
  }

  private def readIRJar(jar: File): Stream[ClassDef] = {
    def isIRFile(path: Path) = {
      val fn = path.getFileName() // null if path is FS root
      fn != null && fn.toString().endsWith(".sjsir")
    }

    // Open zip/jar file as filesystem.
    // The type ascription is necessary on JDK 13+.
    val fs = FileSystems.newFileSystem(jar.toPath(), null: ClassLoader)
    StreamSupport
      .stream(fs.getRootDirectories().spliterator(), /* parallel= */ false)
      .flatMap(Files.walk(_))
      .filter(isIRFile(_))
      .map[ClassDef](readIR(_))
      .onClose(() => fs.close()) // only close fs once all IR is read.
  }

  private def writeIRFile(file: File, tree: ClassDef): Unit = {
    Files.createDirectories(file.toPath().getParent())
    val outputStream =
      new BufferedOutputStream(new FileOutputStream(file))
    try {
      Serializers.serialize(outputStream, tree)
    } finally {
      outputStream.close()
    }
  }

  private def getJSTypes(trees: Stream[ClassDef]): JSTypes = {
    trees.filter(_.kind.isJSType).reduce[JSTypes](
        Map.empty, (m, v) => m.updated(v.className, v.jsNativeLoadSpec), _ ++ _)
  }

  private def cleanTree(tree: ClassDef, jsTypes: JSTypes,
      errorManager: ErrorManager): ClassDef = {
    new ClassDefCleaner(tree.className, jsTypes, errorManager)
      .cleanClassDef(tree)
  }

  private final class ClassDefCleaner(enclosingClassName: ClassName,
      jsTypes: JSTypes, errorManager: ErrorManager)
      extends Transformers.ClassTransformer {

    def cleanClassDef(tree: ClassDef): ClassDef = {
      import tree._

      // Preprocess the super interface list
      val newInterfaces = transformInterfaceList(interfaces)

      /* Remove the `private def writeReplace__O` generated by scalac 2.13+
       * in the companion of serializable classes.
       */
      val newMethods = methods.filter(_.name.name != writeReplaceMethodName)

      val preprocessedTree = ClassDef(name, originalName, kind, jsClassCaptures,
          superClass, newInterfaces, jsSuperClass, jsNativeLoadSpec, fields,
          newMethods, jsConstructor, jsMethodProps, jsNativeMembers,
          topLevelExportDefs)(
          optimizerHints)(pos)

      // Only validate the hierarchy; do not transform
      validateClassName(preprocessedTree.name.name)
      for (superClass <- preprocessedTree.superClass)
        validateClassName(superClass.name)
      for (interface <- preprocessedTree.interfaces)
        validateClassName(interface.name)

      val transformedClassDef =
        Hashers.hashClassDef(eliminateRedundantBridges(this.transformClassDef(preprocessedTree)))

      postTransformChecks(transformedClassDef)
      transformedClassDef
    }

    private def transformInterfaceList(
        interfaces: List[ClassIdent]): List[ClassIdent] = {

      /* Replace references to scala.Serializable by java.io.Serializable.
       * This works around the fact that scalac adds scala.Serializable to the
       * companion object of any class that extends java.io.Serializable.
       */

      if (!interfaces.exists(_.name == ScalaSerializable)) {
        interfaces
      } else if (interfaces.exists(_.name == JavaIOSerializable)) {
        interfaces.filter(_.name != ScalaSerializable)
      } else {
        interfaces.map { ident =>
          if (ident.name == ScalaSerializable)
            ClassIdent(JavaIOSerializable)(ident.pos)
          else
            ident
        }
      }
    }

    override def transformAnyFieldDef(fieldDef: AnyFieldDef): AnyFieldDef = {
      super.transformAnyFieldDef(fieldDef) match {
        case m @ FieldDef(flags, name, originalName, ftpe) =>
          implicit val pos = m.pos
          FieldDef(flags, name, originalName, transformType(ftpe))

        case f: JSFieldDef => f
      }
    }

    override def transformMethodDef(methodDef: MethodDef): MethodDef = {
      val m = super.transformMethodDef(methodDef)
      val MethodDef(flags, name, originalName, args, resultType, body) = m
      implicit val pos = m.pos
      MethodDef(flags, transformMethodIdent(name), originalName, transformParamDefs(args),
          transformType(resultType), body)(m.optimizerHints, Unversioned)
    }

    /** Eliminate bridges that have become redundant because of our additional erasure. */
    private def eliminateRedundantBridges(classDef: ClassDef): ClassDef = {
      import MemberNamespace._

      def argsCorrespond(args: List[Tree], paramDefs: List[ParamDef]): Boolean = {
        (args.size == paramDefs.size) && args.zip(paramDefs).forall {
          case (VarRef(argName), ParamDef(LocalIdent(paramName), _, _, _)) =>
            argName == paramName
          case _ =>
            false
        }
      }

      // Instance bridges, which call "themselves" (another version of themselves with the same name)

      def isRedundantBridge(method: MethodDef): Boolean = {
        val MethodDef(flags, MethodIdent(name), _, paramDefs, _, body) = method

        flags.namespace == Public && {
          body match {
            case Some(Apply(ApplyFlags.empty, This(), MethodIdent(`name`), args)) =>
              argsCorrespond(args, paramDefs)
            case _ =>
              false
          }
        }
      }

      val newMethods1 = classDef.methods.filterNot(isRedundantBridge(_))

      // Make sure that we did not remove *all* overloads for any method name

      def publicMethodNames(methods: List[MethodDef]): Set[MethodName] = {
        methods
          .withFilter(_.flags.namespace == Public)
          .map(_.name.name)
          .toSet
      }

      val lostMethodNames = publicMethodNames(classDef.methods) -- publicMethodNames(newMethods1)
      if (lostMethodNames.nonEmpty) {
        for (lostMethodName <- lostMethodNames)
          reportError(s"eliminateRedundantBridges removed all overloads of ${lostMethodName.nameString}")(classDef.pos)
      }

      // Static forwarders to redundant bridges -- these are duplicate public static methods

      def isStaticForwarder(memberDef: MethodDef): Boolean = memberDef match {
        case MethodDef(flags, MethodIdent(name), _, paramDefs, _, Some(body)) if flags.namespace == PublicStatic =>
          body match {
            case Apply(ApplyFlags.empty, LoadModule(_), MethodIdent(`name`), args) =>
              argsCorrespond(args, paramDefs)
            case _ =>
              false
          }
        case _ =>
          false
      }

      val seenStaticForwarderNames = mutable.Set.empty[MethodName]
      val newMethods2 = newMethods1.filter { m =>
        if (isStaticForwarder(m))
          seenStaticForwarderNames.add(m.name.name) // keep if it is the first one
        else
          true // always keep
      }

      new ClassDef(
        classDef.name,
        classDef.originalName,
        classDef.kind,
        classDef.jsClassCaptures,
        classDef.superClass,
        classDef.interfaces,
        classDef.jsSuperClass,
        classDef.jsNativeLoadSpec,
        classDef.fields,
        newMethods2,
        classDef.jsConstructor,
        classDef.jsMethodProps,
        classDef.jsNativeMembers,
        classDef.topLevelExportDefs
      )(classDef.optimizerHints)(classDef.pos)
    }

    private def transformParamDefs(paramDefs: List[ParamDef]): List[ParamDef] = {
      for (paramDef <- paramDefs) yield {
        implicit val pos = paramDef.pos
        val ParamDef(name, originalName, ptpe, mutable) = paramDef
        ParamDef(name, originalName, transformType(ptpe), mutable)
      }
    }

    override def transform(tree: Tree): Tree = {
      implicit val pos = tree.pos

      val tree1 = preTransform(tree)
      val tree2 = if (tree1 eq tree) super.transform(tree) else transform(tree1)
      val result = postTransform(tree2)

      if (transformType(result.tpe) != result.tpe)
        reportError(s"the result type of a ${result.getClass().getSimpleName()} was not transformed")

      result
    }

    private def preTransform(tree: Tree): Tree = {
      implicit val pos = tree.pos

      tree match {
        // <= 2.12 : toJSVarArgs(jsArrayOps(jsArray).toSeq) -> jsArray
        case IntrinsicCall(ScalaJSRuntimeMod, `toJSVarArgsReadOnlyMethodName`,
            List(Apply(
                ApplyFlags.empty,
                IntrinsicCall(JSAnyMod, `jsArrayOpsToArrayOpsMethodName`, List(jsArray)),
                MethodIdent(`toReadOnlySeqMethodName`),
                Nil)
            )) =>
          jsArray

        // >= 2.13 : toJSVarArgs(toSeq$extension(jsArray)) -> jsArray
        case IntrinsicCall(ScalaJSRuntimeMod, `toJSVarArgsImmutableMethodName`,
            List(IntrinsicCall(JSArrayOpsMod, `toImmutableSeqExtensionMethodName`, List(jsArray)))) =>
          jsArray

        case IntrinsicCall(JSAnyMod, `jsAnyFromIntMethodName`, List(arg)) =>
          arg
        case IntrinsicCall(JSAnyMod, `jsAnyFromStringMethodName`, List(arg)) =>
          arg
        case IntrinsicCall(JSAnyMod, `jsArrayOpsToArrayMethodName`, List(arg)) =>
          arg
        case IntrinsicCall(JSDynamicImplicitsMod, `number2dynamicMethodName`, List(arg)) =>
          arg
        case IntrinsicCall(JSNumberOpsMod, `enableJSNumberOpsDoubleMethodName`, List(arg)) =>
          arg
        case IntrinsicCall(JSNumberOpsMod, `enableJSNumberOpsIntMethodName`, List(arg)) =>
          arg
        case IntrinsicCall(JSStringOpsMod, `enableJSStringOpsMethodName`, List(arg)) =>
          arg
        case IntrinsicCall(UnionTypeMod, `unionTypeFromMethodName`, List(arg, _)) =>
          arg

        case IntrinsicCall(JSDynamicImplicitsMod, `truthValueMethodName`, List(arg)) =>
          AsInstanceOf(
              JSUnaryOp(JSUnaryOp.!, JSUnaryOp(JSUnaryOp.!, arg)),
              BooleanType)

        // LinkingInfo
        // Must stay in sync with the definitions in `scala.scalajs.LinkingInfo`
        case IntrinsicCall(LinkingInfoClass, `esVersionMethodName`, Nil) =>
          LinkTimeProperty(LinkTimeProperty.ESVersion)(IntType)

        case IntrinsicCall(LinkingInfoClass, `isWebAssemblyMethodName`, Nil) =>
          LinkTimeProperty(LinkTimeProperty.IsWebAssembly)(BooleanType)

        case IntrinsicCall(LinkingInfoClass, `linkerVersionMethodName`, Nil) =>
          LinkTimeProperty(LinkTimeProperty.LinkerVersion)(StringType)

        case _ =>
          tree
      }
    }

    private object IntrinsicCall {
      def unapply(tree: Apply): Option[(ClassName, MethodName, List[Tree])] = tree match {
        case Apply(ApplyFlags.empty, LoadModule(moduleClassName), MethodIdent(methodName), args) =>
          Some(moduleClassName, methodName, args)
        case _ =>
          None
      }
    }

    private object ScalaVarArgsReadOnlyLiteral {
      def unapply(tree: Apply): Option[List[Tree]] = tree match {
        case IntrinsicCall(ScalaJSRuntimeMod, `toScalaVarArgsReadOnlyMethodName`,
            List(JSArrayConstr(args))) =>
          if (args.forall(_.isInstanceOf[Tree]))
            Some(args.map(_.asInstanceOf[Tree]))
          else
            None
        case _ =>
          None
      }
    }

    private def postTransform(tree: Tree): Tree = {
      implicit val pos = tree.pos

      tree match {
        case VarDef(name, originalName, vtpe, mutable, rhs) =>
          VarDef(name, originalName, transformType(vtpe), mutable, rhs)

        case Labeled(label, tpe, body) =>
          Labeled(label, transformType(tpe), body)
        case If(cond, thenp, elsep) =>
          If(cond, thenp, elsep)(transformType(tree.tpe))
        case TryCatch(block, errVar, errVarOriginalName, handler) =>
          TryCatch(block, errVar, errVarOriginalName, handler)(transformType(tree.tpe))
        case Match(selector, cases, default) =>
          Match(selector, cases, default)(transformType(tree.tpe))

        case New(className, ctor, args) =>
          New(transformNonJSClassName(className), transformMethodIdent(ctor), args)
        case Select(qualifier, field @ FieldIdent(fieldName)) =>
          val newFieldName = FieldName(transformNonJSClassName(fieldName.className), fieldName.simpleName)
          Select(qualifier, FieldIdent(newFieldName)(field.pos))(transformType(tree.tpe))

        case t: Apply =>
          Apply(t.flags, t.receiver, transformMethodIdent(t.method), t.args)(
              transformType(t.tpe))
        case t: ApplyStatically =>
          ApplyStatically(t.flags, t.receiver,
              transformNonJSClassName(t.className),
              transformMethodIdent(t.method), t.args)(transformType(t.tpe))
        case t: ApplyStatic =>
          ApplyStatic(t.flags, transformNonJSClassName(t.className),
              transformMethodIdent(t.method), t.args)(transformType(t.tpe))

        case NewArray(typeRef, lengths) =>
          NewArray(transformArrayTypeRef(typeRef), lengths)
        case ArrayValue(typeRef, elems) =>
          ArrayValue(transformArrayTypeRef(typeRef), elems)
        case ArraySelect(array, index) =>
          ArraySelect(array, index)(transformType(tree.tpe))

        case IsInstanceOf(expr, testType) =>
          IsInstanceOf(expr, transformType(testType))
        case AsInstanceOf(expr, tpe) =>
          AsInstanceOf(expr, transformType(tpe))

        case LoadJSConstructor(className) =>
          genLoadFromLoadSpecOf(className)
        case LoadJSModule(className) =>
          genLoadFromLoadSpecOf(className)

        case t: ClassOf =>
          if (transformTypeRef(t.typeRef) != t.typeRef)
            reportError(s"illegal ClassOf(${t.typeRef})")
          t

        case t @ VarRef(ident) =>
          VarRef(ident)(transformType(t.tpe))

        case Closure(flags, captureParams, params, restParam, resultType, body, captureValues) =>
          Closure(flags, transformParamDefs(captureParams), transformParamDefs(params),
              restParam, resultType, body, captureValues)

        case _ =>
          tree
      }
    }

    private def genLoadFromLoadSpecOf(className: ClassName)(
        implicit pos: Position): Tree = {
      jsTypes.get(className) match {
        case Some(Some(loadSpec)) =>
          genLoadFromLoadSpec(loadSpec)
        case Some(None) =>
          reportError(
              s"${className.nameString} does not have a load spec " +
              "(this shouldn't have happened at all; bug in the compiler?)")
          JSGlobalRef("Object")
        case None =>
          reportError(s"${className.nameString} is not a JS type")
          JSGlobalRef("Object")
      }
    }

    private def genLoadFromLoadSpec(loadSpec: JSNativeLoadSpec)(
        implicit pos: Position): Tree = {
      loadSpec match {
        case JSNativeLoadSpec.Global(globalRef, Nil) =>
          JSGlobalRef(globalRef)
        case _ =>
          reportError(
              s"unsupported load spec $loadSpec; " +
              "only @JSGlobal without `.` is supported")
          JSGlobalRef("Object")
      }
    }

    private def transformMethodIdent(ident: MethodIdent): MethodIdent = {
      implicit val pos = ident.pos
      MethodIdent(transformMethodName(ident.name))
    }

    private def transformClassRef(cls: ClassRef)(
        implicit pos: Position): ClassRef = {
      if (jsTypes.contains(cls.className))
        ClassRef(ObjectClass)
      else
        ClassRef(transformClassName(cls.className))
    }

    private def transformArrayTypeRef(typeRef: ArrayTypeRef)(
        implicit pos: Position): ArrayTypeRef = {
      typeRef.base match {
        case _: PrimRef =>
          typeRef
        case ClassRef(baseClassName) =>
          if (jsTypes.contains(baseClassName))
            ArrayTypeRef(ClassRef(ObjectClass), typeRef.dimensions)
          else
            ArrayTypeRef(ClassRef(transformClassName(baseClassName)), typeRef.dimensions)
      }
    }

    private def transformTypeRef(typeRef: TypeRef)(
        implicit pos: Position): TypeRef = typeRef match {
      case typeRef: PrimRef          => typeRef
      case typeRef: ClassRef         => transformClassRef(typeRef)
      case typeRef: ArrayTypeRef     => transformArrayTypeRef(typeRef)
      case typeRef: TransientTypeRef => TransientTypeRef(typeRef.name)(transformType(typeRef.tpe))
    }

    private def postTransformChecks(classDef: ClassDef): Unit = {
      // Check that no two methods have been erased to the same name
      val seenMethodNames = mutable.Set.empty[(MemberNamespace, MethodName)]
      classDef.methods.foreach { m =>
        if (!seenMethodNames.add((m.flags.namespace, m.name.name))) {
          reportError(
              s"duplicate method name ${m.name.name.nameString} after erasure")(
              m.pos)
        }
      }
    }

    private def transformType(tpe: Type)(implicit pos: Position): Type = {
      tpe match {
        case ClassType(ObjectClass, _) =>
          // In java.lang.Object iself, there are ClassType(ObjectClass) that must be preserved as is.
          tpe
        case ClassType(cls, nullable) =>
          transformClassName(cls) match {
            case ObjectClass => if (nullable) AnyType else AnyNotNullType
            case newCls      => ClassType(newCls, nullable)
          }
        case ArrayType(arrayTypeRef, nullable) =>
          ArrayType(transformArrayTypeRef(arrayTypeRef), nullable)
        case ClosureType(paramTypes, resultType, nullable) =>
          ClosureType(paramTypes.map(transformType(_)), transformType(resultType), nullable)
        case AnyType | AnyNotNullType | _:PrimType | _:RecordType =>
          tpe
      }
    }

    private def transformClassName(cls: ClassName)(implicit pos: Position): ClassName = {
      ClassNameSubstitutions.getOrElse(cls, {
        validateClassName(cls)
        cls
      })
    }

    private def validateClassName(cls: ClassName)(implicit pos: Position): Unit = {
      def isJavaScriptExceptionWithinItself =
        cls == JavaScriptExceptionClass && enclosingClassName == JavaScriptExceptionClass

      def isTypedArrayBufferBridgeWithinItself = {
        (cls == TypedArrayBufferBridge || cls == TypedArrayBufferBridgeMod) &&
        (enclosingClassName == TypedArrayBufferBridge || enclosingClassName == TypedArrayBufferBridgeMod)
      }

      def isAnException: Boolean =
        isJavaScriptExceptionWithinItself || isTypedArrayBufferBridgeWithinItself

      if (cls.nameString.startsWith("scala.") && !isAnException)
        reportError(s"Illegal reference to Scala class ${cls.nameString}")
    }

    private def transformNonJSClassName(cls: ClassName)(implicit pos: Position): ClassName = {
      if (jsTypes.contains(cls)) {
        reportError(s"Invalid reference to JS class ${cls.nameString}")
        cls
      } else {
        transformClassName(cls)
      }
    }

    private def transformMethodName(name: MethodName)(implicit pos: Position): MethodName = {
      MethodName(name.simpleName, name.paramTypeRefs.map(transformTypeRef),
          transformTypeRef(name.resultTypeRef), name.isReflectiveProxy)
    }

    private def reportError(msg: String)(implicit pos: Position): Unit = {
      errorManager.reportError(s"$msg in ${enclosingClassName.nameString}")
    }
  }
}

object JavalibIRCleaner {
  // Within js.JavaScriptException, which is part of the linker private lib, we can refer to itself
  private val JavaScriptExceptionClass = ClassName("scala.scalajs.js.JavaScriptException")

  // Within TypedArrayBufferBridge, which is actually part of the library, we can refer to itself
  private val TypedArrayBufferBridge = ClassName("scala.scalajs.js.typedarray.TypedArrayBufferBridge")
  private val TypedArrayBufferBridgeMod = ClassName("scala.scalajs.js.typedarray.TypedArrayBufferBridge$")

  private val ImmutableSeq = ClassName("scala.collection.immutable.Seq")
  private val JavaIOSerializable = ClassName("java.io.Serializable")
  private val JSAny = ClassName("scala.scalajs.js.Any")
  private val JSAnyMod = ClassName("scala.scalajs.js.Any$")
  private val JSArray = ClassName("scala.scalajs.js.Array")
  private val JSArrayOps = ClassName("scala.scalajs.js.ArrayOps")
  private val JSArrayOpsMod = ClassName("scala.scalajs.js.ArrayOps$")
  private val JSDynamic = ClassName("scala.scalajs.js.Dynamic")
  private val JSDynamicImplicitsMod = ClassName("scala.scalajs.js.DynamicImplicits$")
  private val JSNumberOps = ClassName("scala.scalajs.js.JSNumberOps")
  private val JSNumberOpsMod = ClassName("scala.scalajs.js.JSNumberOps$")
  private val JSStringOps = ClassName("scala.scalajs.js.JSStringOps")
  private val JSStringOpsMod = ClassName("scala.scalajs.js.JSStringOps$")
  private val ReadOnlySeq = ClassName("scala.collection.Seq")
  private val ScalaSerializable = ClassName("scala.Serializable")
  private val ScalaJSRuntimeMod = ClassName("scala.scalajs.runtime.package$")
  private val UnionType = ClassName("scala.scalajs.js.$bar")
  private val UnionTypeMod = ClassName("scala.scalajs.js.$bar$")
  private val UnionTypeEvidence = ClassName("scala.scalajs.js.$bar$Evidence")
  private val LinkingInfoClass = ClassName("scala.scalajs.LinkingInfo$")

  private val enableJSNumberOpsDoubleMethodName =
    MethodName("enableJSNumberOps", List(DoubleRef), ClassRef(JSNumberOps))
  private val enableJSNumberOpsIntMethodName =
    MethodName("enableJSNumberOps", List(IntRef), ClassRef(JSNumberOps))
  private val enableJSStringOpsMethodName =
    MethodName("enableJSStringOps", List(ClassRef(BoxedStringClass)), ClassRef(JSStringOps))
  private val jsAnyFromIntMethodName =
    MethodName("fromInt", List(IntRef), ClassRef(JSAny))
  private val jsAnyFromStringMethodName =
    MethodName("fromString", List(ClassRef(BoxedStringClass)), ClassRef(JSAny))
  private val jsArrayOpsToArrayMethodName =
    MethodName("jsArrayOps", List(ClassRef(JSArray)), ClassRef(JSArray))
  private val jsArrayOpsToArrayOpsMethodName =
    MethodName("jsArrayOps", List(ClassRef(JSArray)), ClassRef(JSArrayOps))
  private val number2dynamicMethodName =
    MethodName("number2dynamic", List(DoubleRef), ClassRef(JSDynamic))
  private val toImmutableSeqExtensionMethodName =
    MethodName("toSeq$extension", List(ClassRef(JSArray)), ClassRef(ImmutableSeq))
  private val toJSVarArgsImmutableMethodName =
    MethodName("toJSVarArgs", List(ClassRef(ImmutableSeq)), ClassRef(JSArray))
  private val toJSVarArgsReadOnlyMethodName =
    MethodName("toJSVarArgs", List(ClassRef(ReadOnlySeq)), ClassRef(JSArray))
  private val toScalaVarArgsReadOnlyMethodName =
    MethodName("toScalaVarArgs", List(ClassRef(JSArray)), ClassRef(ReadOnlySeq))
  private val toReadOnlySeqMethodName =
    MethodName("toSeq", Nil, ClassRef(ReadOnlySeq))
  private val truthValueMethodName =
    MethodName("truthValue", List(ClassRef(JSDynamic)), BooleanRef)
  private val unionTypeFromMethodName =
    MethodName("from", List(ClassRef(ObjectClass), ClassRef(UnionTypeEvidence)), ClassRef(UnionType))
  private val writeReplaceMethodName =
    MethodName("writeReplace", Nil, ClassRef(ObjectClass))

  // LinkingInfo
  private val esVersionMethodName = MethodName("esVersion", Nil, IntRef)
  private val isWebAssemblyMethodName = MethodName("isWebAssembly", Nil, BooleanRef)
  private val linkerVersionMethodName = MethodName("linkerVersion", Nil, ClassRef(BoxedStringClass))

  private val ClassNameSubstitutions: Map[ClassName, ClassName] = {
    val refBaseNames =
      List("Boolean", "Char", "Byte", "Short", "Int", "Long", "Float", "Double", "Object")
    val refPairs = for {
      refBaseName <- refBaseNames
    } yield {
      val simpleName = refBaseName + "Ref"
      ClassName("scala.runtime." + simpleName) -> ClassName("java.util.internal." + simpleName)
    }

    val tuplePairs = for {
      n <- (2 to 22).toList
    } yield {
      ClassName("scala.Tuple" + n) -> ClassName("java.util.internal.Tuple" + n)
    }

    val otherPairs = List(
      /* AssertionError conveniently features a constructor taking an Object.
       * Since any MatchError in the javalib would be a bug, it is fine to
       * rewrite them to AssertionErrors.
       */
      ClassName("scala.MatchError") -> ClassName("java.lang.AssertionError"),
    )

    val allPairs = refPairs ++ tuplePairs ++ otherPairs
    allPairs.toMap
  }
}
