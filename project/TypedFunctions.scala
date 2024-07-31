package build

import java.io.ByteArrayOutputStream

import org.scalajs.ir
import org.scalajs.ir._
import org.scalajs.ir.Names._
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._
import org.scalajs.ir.Position.NoPosition
import org.scalajs.ir.Version.Unversioned

/** Hard-coded IR for the scala.scalajs.runtime.TypedFunctionN classes.
 *
 *  We cannot write them in user-space because we have no user syntax to
 *  generate typed closures and their closure types.
 */
object TypedFunctions {
  private def makeClassDef(arity: Int): ClassDef = {
    implicit val DummyPos = NoPosition

    val className = ClassName("scala.scalajs.runtime.TypedFunction" + arity)
    val thisType = ClassType(className)

    val superClassName = ClassName("scala.runtime.AbstractFunction" + arity)

    val ObjectClassRef = ClassRef(ObjectClass)
    val ClassClassRef = ClassRef(ClassClass)
    val StringClassRef = ClassRef(BoxedStringClass)

    val closureTypeRef = ClosureTypeRef(List.fill(arity)(ObjectClassRef), ObjectClassRef)
    val closureType = ClosureType(List.fill(arity)(AnyType), AnyType)

    val fParamIdent = LocalIdent(LocalName("f"))
    val fFieldIdent = FieldIdent(FieldName(className, SimpleFieldName("f")))

    val fFieldSelect = Select(This()(thisType), fFieldIdent)(closureType)

    val xParamIdents = List.tabulate(arity)(i => LocalIdent(LocalName("x" + (i + 1))))

    val EAF = ApplyFlags.empty

    val classDef = ClassDef(
      ClassIdent(className),
      NoOriginalName,
      ClassKind.Class,
      None,
      Some(ClassIdent(superClassName)),
      Nil,
      None,
      None,
      fields = List(
        FieldDef(MemberFlags.empty, fFieldIdent, NoOriginalName, closureType),
      ),
      methods = List(
        /* def this(f: closureType) = { super(); this.f = f; } */
        MethodDef(
          MemberFlags.empty.withNamespace(MemberNamespace.Constructor),
          MethodIdent(MethodName.constructor(List(closureTypeRef))),
          NoOriginalName,
          List(ParamDef(fParamIdent, NoOriginalName, closureType, mutable = false)),
          NoType,
          Some(Block(
            ApplyStatically(EAF.withConstructor(true), This()(thisType),
                superClassName, MethodIdent(NoArgConstructorName), Nil)(NoType),
            Assign(fFieldSelect, VarRef(fParamIdent)(closureType))
          )))(OptimizerHints.empty, Unversioned),

        /* def apply(x1: any, ..., xn: any): any = this.f(x1, ..., xn) */
        MethodDef(
          MemberFlags.empty,
          MethodIdent(MethodName("apply", List.fill(arity)(ObjectClassRef), ObjectClassRef)),
          NoOriginalName,
          xParamIdents.map(ident => ParamDef(ident, NoOriginalName, AnyType, mutable = false)),
          AnyType,
          Some {
            ApplyTypedClosure(
              EAF,
              fFieldSelect,
              xParamIdents.map(ident => VarRef(ident)(AnyType))
            )
          })(OptimizerHints.empty.withInline(true), Unversioned),

      ),
      jsConstructor = None,
      jsMethodProps = Nil,
      jsNativeMembers = Nil,
      topLevelExportDefs = Nil)(
      OptimizerHints.empty.withInline(true))

    Hashers.hashClassDef(classDef)
  }

  def makeIRBytes(arity: Int): Array[Byte] = {
    val stream = new ByteArrayOutputStream
    try ir.Serializers.serialize(stream, makeClassDef(arity))
    finally stream.close()
    stream.toByteArray
  }
}
