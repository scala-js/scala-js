package build

import org.scalajs.ir._
import org.scalajs.ir.ClassKind
import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import java.io._
import java.nio.file.{Files, Path}
import java.{util => ju}

import scala.collection.mutable

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
 *
 *  Afterwards, we check that the IR does not contain any reference to classes
 *  undef the `scala.*` package, with the unfortunate exception of
 *  `scala.scalajs.runtime.UndefinedBehaviorError`.
 */
object JavalibIRCleaner {
  private val ThrowableClass = ClassName("java.lang.Throwable")
  private val JavaIOSerializable = ClassName("java.io.Serializable")
  private val JavaLangSemanticsUtils = ClassName("java.lang.SemanticsUtils$")
  private val ScalaJSRuntimePackage = ClassName("scala.scalajs.runtime.package$")
  private val ScalaJSUndefinedBehaviorError = ClassName("scala.scalajs.runtime.UndefinedBehaviorError")
  private val ScalaSerializable = ClassName("scala.Serializable")

  private val unwrapJSExceptionMethodName =
    MethodName("unwrapJavaScriptException", List(ClassRef(ThrowableClass)), ClassRef(ObjectClass))
  private val writeReplaceMethodName =
    MethodName("writeReplace", Nil, ClassRef(ObjectClass))

  def cleanIR(dependencyClasspath: Seq[File], javalibDir: File, outputDir: File,
      logger: Logger): Set[File] = {

    val errorManager = new ErrorManager(logger)

    val javalibDirPath = javalibDir.toPath()
    val outputDirPath = outputDir.toPath()

    val libraryIRFiles = dependencyClasspath.flatMap(f => listIRFiles(f.toPath()))
    val javalibIRFiles = listIRFiles(javalibDirPath)

    val jsTypes = getJSTypes(libraryIRFiles ++ javalibIRFiles)

    val resultBuilder = Set.newBuilder[File]

    for (irFile <- javalibIRFiles) {
      import ClassKind._

      val tree = irFile.tree
      tree.kind match {
        case Class | ModuleClass | Interface | HijackedClass =>
          val cleanedTree = cleanTree(tree, jsTypes, errorManager)
          val outputPath =
            outputDirPath.resolve(javalibDirPath.relativize(irFile.path))
          writeIRFile(outputPath, cleanedTree)
          resultBuilder += outputPath.toFile()

        case AbstractJSType | NativeJSClass | NativeJSModuleClass =>
          // discard

        case JSClass | JSModuleClass =>
          errorManager.reportError(
              s"found non-native JS class ${tree.encodedName}")(tree.pos)
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
    private var _errorCount: Int = 0

    def reportError(msg: String)(implicit pos: Position): Unit = {
      logger.error(s"$msg at $pos")
      _errorCount += 1
    }

    def hasErrors: Boolean = _errorCount != 0

    def errorCount: Int = _errorCount
  }

  private final class IRFile(val path: Path, val tree: ClassDef)

  private def listIRFiles(path: Path): Seq[IRFile] = {
    import java.nio.file._
    import java.nio.file.attribute.BasicFileAttributes

    val builder = Seq.newBuilder[IRFile]

    val dirVisitor = new SimpleFileVisitor[Path] {
      override def visitFile(path: Path, attrs: BasicFileAttributes): FileVisitResult = {
        if (path.getFileName().toString().endsWith(".sjsir"))
          builder += new IRFile(path, readIRFile(path))
        super.visitFile(path, attrs)
      }
    }

    Files.walkFileTree(path, ju.EnumSet.of(FileVisitOption.FOLLOW_LINKS),
        Int.MaxValue, dirVisitor)
    builder.result()
  }

  private def readIRFile(path: Path): ClassDef = {
    import java.nio.ByteBuffer
    import java.nio.channels.FileChannel

    val channel = FileChannel.open(path)
    try {
      val fileSize = channel.size()
      if (fileSize > Int.MaxValue.toLong)
        throw new IOException(s"IR file too large: $path")
      val buffer = ByteBuffer.allocate(fileSize.toInt)
      channel.read(buffer)
      buffer.flip()
      Serializers.deserialize(buffer)
    } finally {
      channel.close()
    }
  }

  private def writeIRFile(path: Path, tree: ClassDef): Unit = {
    Files.createDirectories(path.getParent())
    val outputStream =
      new BufferedOutputStream(new FileOutputStream(path.toFile()))
    try {
      Serializers.serialize(outputStream, tree)
    } finally {
      outputStream.close()
    }
  }

  private def getJSTypes(irFiles: Seq[IRFile]): Map[ClassName, ClassDef] = {
    (for {
      irFile <- irFiles
      if irFile.tree.kind.isJSType
    } yield {
      val tree = irFile.tree
      tree.encodedName -> tree
    }).toMap
  }

  private def cleanTree(tree: ClassDef, jsTypes: Map[ClassName, ClassDef],
      errorManager: ErrorManager): ClassDef = {
    new ClassDefCleaner(tree.encodedName, jsTypes, errorManager)
      .cleanClassDef(tree)
  }

  private final class ClassDefCleaner(enclosingClassName: ClassName,
      jsTypes: Map[ClassName, ClassDef], errorManager: ErrorManager)
      extends Transformers.ClassTransformer {

    def cleanClassDef(tree: ClassDef): ClassDef = {
      import tree._

      val preprocessedTree = {
        var changed = false

        // Preprocess the super interface list
        val newInterfaces = transformInterfaceList(interfaces)
        if (newInterfaces ne interfaces)
          changed = true

        /* Remove the `private def writeReplace__O` generated by scalac 2.13+
         * in the companion of serializable classes.
         */
        val newMemberDefs = memberDefs.filter {
          case MethodDef(_, MethodIdent(`writeReplaceMethodName`, _), _, _, _) =>
            changed = true
            false
          case _ =>
            true
        }

        if (changed) {
          ClassDef(name, kind, jsClassCaptures, superClass, newInterfaces,
              jsSuperClass, jsNativeLoadSpec, newMemberDefs, topLevelExportDefs)(
              optimizerHints)(pos)
        } else {
          tree
        }
      }

      validateClassName(preprocessedTree.name.name)
      for (superClass <- preprocessedTree.superClass)
        validateClassName(superClass.name)
      for (interface <- preprocessedTree.interfaces)
        validateClassName(interface.name)

      val transformedClassDef =
        Hashers.hashClassDef(this.transformClassDef(preprocessedTree))

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

    override def transformMemberDef(memberDef: MemberDef): MemberDef = {
      super.transformMemberDef(memberDef) match {
        case m @ MethodDef(flags, name, args, resultType, body) =>
          MethodDef(flags, transformMethodIdent(name), args,
              resultType, body)(m.optimizerHints, m.hash)(m.pos)
        case m =>
          m
      }
    }

    override def transform(tree: Tree, isStat: Boolean): Tree = {
      implicit val pos = tree.pos

      val preprocessedTree = tree match {
        /* In SemanticsUtils, there is one generic `throw exception()` where
         * `exception()` is one of the exceptions for undefined behavior. We
         * as humans know that none of those can be a `JavaScriptException`,
         * but the compiler doesn't, so it introduces a call to
         * `sjs.runtime.unwrapJavaScriptExeption(arg)`. Here, we get rid of
         * that call and rewrite `tree` to `throw arg`.
         */
        case Throw(Apply(_, LoadModule(ScalaJSRuntimePackage),
            MethodIdent(`unwrapJSExceptionMethodName`, _), arg :: Nil))
            if enclosingClassName == JavaLangSemanticsUtils =>
          Throw(arg)

        case _ =>
          tree
      }

      val result = super.transform(preprocessedTree, isStat) match {
        case New(className, ctor, args) =>
          New(className, transformMethodIdent(ctor), args)

        case t: Apply =>
          Apply(t.flags, t.receiver, transformMethodIdent(t.method),
              t.args)(t.tpe)
        case t: ApplyStatically =>
          validateNonJSClassName(t.className)
          ApplyStatically(t.flags, t.receiver, t.className,
              transformMethodIdent(t.method), t.args)(t.tpe)
        case t: ApplyStatic =>
          validateNonJSClassName(t.className)
          ApplyStatic(t.flags, t.className,
              transformMethodIdent(t.method), t.args)(t.tpe)

        case NewArray(typeRef, lengths) =>
          NewArray(transformArrayTypeRef(typeRef), lengths)
        case ArrayValue(typeRef, elems) =>
          ArrayValue(transformArrayTypeRef(typeRef), elems)

        case t: IsInstanceOf =>
          validateType(t.testType)
          t

        case LoadJSConstructor(className) =>
          genLoadFromLoadSpecOf(className)
        case LoadJSModule(className) =>
          genLoadFromLoadSpecOf(className)

        case t: ClassOf =>
          if (transformTypeRef(t.typeRef) != t.typeRef)
            reportError(s"illegal Class(${t.typeRef})")
          t

        case t =>
          t
      }

      validateType(result.tpe)
      result
    }

    private def genLoadFromLoadSpecOf(className: ClassName)(
        implicit pos: Position): Tree = {
      jsTypes.get(className) match {
        case Some(classDef) =>
          classDef.jsNativeLoadSpec match {
            case Some(loadSpec) =>
              genLoadFromLoadSpec(loadSpec)
            case None =>
              reportError(
                  s"${className.nameString} does not have a load spec " +
                  "(this shouldn't have happened at all; bug in the compiler?)")
              JSGlobalRef("Object")
          }
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
      val encodedName = ident.name
      val paramTypeRefs = encodedName.paramTypeRefs
      val newParamTypeRefs = paramTypeRefs.map(transformTypeRef)
      val resultTypeRef = encodedName.resultTypeRef
      val newResultTypeRef = resultTypeRef.map(transformTypeRef)
      if (newParamTypeRefs == paramTypeRefs && newResultTypeRef == resultTypeRef) {
        ident
      } else {
        MethodIdent(
            MethodName(encodedName.simpleName, newParamTypeRefs, newResultTypeRef),
            ident.originalName)(ident.pos)
      }
    }

    private def transformClassRef(cls: ClassRef)(
        implicit pos: Position): ClassRef = {
      if (jsTypes.contains(cls.className)) {
        ClassRef(ObjectClass)
      } else {
        validateClassName(cls.className)
        cls
      }
    }

    private def transformArrayTypeRef(typeRef: ArrayTypeRef)(
        implicit pos: Position): ArrayTypeRef = {
      typeRef.base match {
        case _: PrimRef =>
          typeRef
        case ClassRef(baseClassName) =>
          if (jsTypes.contains(baseClassName)) {
            ArrayTypeRef(ClassRef(ObjectClass), typeRef.dimensions)
          } else {
            validateClassName(baseClassName)
            typeRef
          }
      }
    }

    private def transformTypeRef(typeRef: TypeRef)(
        implicit pos: Position): TypeRef = typeRef match {
      case typeRef: PrimRef      => typeRef
      case typeRef: ClassRef     => transformClassRef(typeRef)
      case typeRef: ArrayTypeRef => transformArrayTypeRef(typeRef)
    }

    private def postTransformChecks(classDef: ClassDef): Unit = {
      // Check that no two methods have been erased to the same name
      val seenMethodNames = mutable.Set.empty[(MemberNamespace, MethodName)]
      for (m <- classDef.memberDefs) {
        m match {
          case MethodDef(flags, name, _, _, _) =>
            if (!seenMethodNames.add((flags.namespace, name.name))) {
              reportError(
                  s"duplicate method name ${name.name.nameString} after erasure")(
                  m.pos)
            }
          case _ =>
        }
      }
    }

    private def validateType(tpe: Type)(implicit pos: Position): Unit = {
      tpe match {
        case ClassType(cls) =>
          validateClassName(cls)
        case ArrayType(ArrayTypeRef(ClassRef(cls), _)) =>
          validateClassName(cls)
        case _ =>
          // ok
      }
    }

    private def validateClassName(cls: ClassName)(implicit pos: Position): Unit = {
      if (isScalaClassName(cls))
        reportError(s"Illegal reference to Scala class ${cls.nameString}")
    }

    private def validateNonJSClassName(cls: ClassName)(implicit pos: Position): Unit = {
      if (jsTypes.contains(cls))
        reportError(s"Invalid reference to JS class ${cls.nameString}")
      else
        validateClassName(cls)
    }

    private def isScalaClassName(cls: ClassName): Boolean = {
      cls.nameString.startsWith("scala.") &&
      cls != ScalaJSUndefinedBehaviorError // TODO We need to get rid of this
    }

    private def reportError(msg: String)(implicit pos: Position): Unit = {
      errorManager.reportError(s"$msg in $enclosingClassName")
    }
  }
}
