package build

/*
 * Hard-coded IR for java.lang.Object.
 */

import java.io.ByteArrayOutputStream

import org.scalajs.ir
import org.scalajs.ir._
import org.scalajs.ir.Names._
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._
import org.scalajs.ir.Position.NoPosition
import org.scalajs.ir.Version.Unversioned

/** Hard-coded IR for java.lang.Object.
 *  We cannot so much as begin to fake a compilation of java.lang.Object,
 *  because Object is hijacked so much by scalac itself that it does not like
 *  at all to try to compile that class. So we have to bypass entirely the
 *  compiler to define java.lang.Object.
 */
object JavaLangObject {
  private val TheClassDef = {
    implicit val DummyPos = NoPosition

    // ClassType(Object) is normally invalid, but not in this class def
    val ThisType = ClassType(ObjectClass, nullable = false)

    val ObjectClassRef = ClassRef(ObjectClass)
    val ClassClassRef = ClassRef(ClassClass)
    val StringClassRef = ClassRef(BoxedStringClass)

    val EAF = ApplyFlags.empty

    val classDef = ClassDef(
      ClassIdent(ObjectClass),
      NoOriginalName,
      ClassKind.Class,
      None,
      None,
      Nil,
      None,
      None,
      fields = Nil,
      List(
        /* def this() = () */
        MethodDef(
          MemberFlags.empty.withNamespace(MemberNamespace.Constructor),
          MethodIdent(NoArgConstructorName),
          NoOriginalName,
          Nil,
          VoidType,
          Some(Skip()))(OptimizerHints.empty, Unversioned),

        /* def getClass(): java.lang.Class[_] = <getclass>(this) */
        MethodDef(
          MemberFlags.empty,
          MethodIdent(MethodName("getClass", Nil, ClassClassRef)),
          NoOriginalName,
          Nil,
          ClassType(ClassClass, nullable = true),
          Some {
            UnaryOp(UnaryOp.GetClass, This()(ThisType))
          })(OptimizerHints.empty.withInline(true), Unversioned),

        /* def hashCode(): Int = <identityHashCode>(this) */
        MethodDef(
          MemberFlags.empty,
          MethodIdent(MethodName("hashCode", Nil, IntRef)),
          NoOriginalName,
          Nil,
          IntType,
          Some {
            UnaryOp(UnaryOp.IdentityHashCode, This()(ThisType))
          })(OptimizerHints.empty.withInline(true), Unversioned),

        /* def equals(that: Object): Boolean = this eq that */
        MethodDef(
          MemberFlags.empty,
          MethodIdent(MethodName("equals", List(ObjectClassRef), BooleanRef)),
          NoOriginalName,
          List(ParamDef(LocalIdent(LocalName("that")), NoOriginalName, AnyType,
            mutable = false)),
          BooleanType,
          Some {
            BinaryOp(BinaryOp.===,
              This()(ThisType),
              VarRef(LocalName("that"))(AnyType))
          })(OptimizerHints.empty.withInline(true), Unversioned),

        /* protected def clone(): Object =
         *   if (this.isInstanceOf[Cloneable]) <clone>(this.asInstanceOf[Cloneable])
         *   else throw new CloneNotSupportedException()
         */
        MethodDef(
          MemberFlags.empty,
          MethodIdent(MethodName("clone", Nil, ObjectClassRef)),
          NoOriginalName,
          Nil,
          AnyType,
          Some {
            If(IsInstanceOf(This()(ThisType), ClassType(CloneableClass, nullable = false)), {
              UnaryOp(UnaryOp.Clone, UnaryOp(UnaryOp.CheckNotNull,
                  AsInstanceOf(This()(ThisType), ClassType(CloneableClass, nullable = true))))
            }, {
              UnaryOp(UnaryOp.Throw, New(ClassName("java.lang.CloneNotSupportedException"),
                MethodIdent(NoArgConstructorName), Nil))
            })(AnyType)
          })(OptimizerHints.empty.withInline(true), Unversioned),

        /* def toString(): String =
         *   getClass().getName() + "@" + Integer.toHexString(hashCode())
         */
        MethodDef(
          MemberFlags.empty,
          MethodIdent(MethodName("toString", Nil, StringClassRef)),
          NoOriginalName,
          Nil,
          ClassType(BoxedStringClass, nullable = true),
          Some {
            BinaryOp(BinaryOp.String_+, BinaryOp(BinaryOp.String_+,
              Apply(
                EAF,
                Apply(EAF, This()(ThisType),
                  MethodIdent(MethodName("getClass", Nil, ClassClassRef)), Nil)(
                  ClassType(ClassClass, nullable = true)),
                MethodIdent(MethodName("getName", Nil, StringClassRef)), Nil)(
                ClassType(BoxedStringClass, nullable = true)),
              // +
              StringLiteral("@")),
              // +
              Apply(
                EAF,
                LoadModule(ClassName("java.lang.Integer$")),
                MethodIdent(MethodName("toHexString", List(IntRef), StringClassRef)),
                List(Apply(EAF, This()(ThisType), MethodIdent(MethodName("hashCode", Nil, IntRef)), Nil)(IntType)))(
                ClassType(BoxedStringClass, nullable = true)))
          })(OptimizerHints.empty, Unversioned),

        /* Since wait() is not supported in any way, a correct implementation
         * of notify() and notifyAll() is to do nothing.
         */

        /* def notify(): Unit = () */
        MethodDef(
          MemberFlags.empty,
          MethodIdent(MethodName("notify", Nil, VoidRef)),
          NoOriginalName,
          Nil,
          VoidType,
          Some(Skip()))(OptimizerHints.empty, Unversioned),

        /* def notifyAll(): Unit = () */
        MethodDef(
          MemberFlags.empty,
          MethodIdent(MethodName("notifyAll", Nil, VoidRef)),
          NoOriginalName,
          Nil,
          VoidType,
          Some(Skip()))(OptimizerHints.empty, Unversioned),

        /* def finalize(): Unit = () */
        MethodDef(
          MemberFlags.empty,
          MethodIdent(MethodName("finalize", Nil, VoidRef)),
          NoOriginalName,
          Nil,
          VoidType,
          Some(Skip()))(OptimizerHints.empty, Unversioned),
      ),
      jsConstructor = None,
      jsMethodProps = List(
        /* JSExport for toString(). */
        JSMethodDef(
          MemberFlags.empty,
          StringLiteral("toString"),
          Nil, None,
          {
            Apply(EAF, This()(ThisType),
                MethodIdent(MethodName("toString", Nil, StringClassRef)),
                Nil)(ClassType(BoxedStringClass, nullable = true))
          })(OptimizerHints.empty, Unversioned)
      ),
      jsNativeMembers = Nil,
      topLevelExportDefs = Nil)(OptimizerHints.empty)

    Hashers.hashClassDef(classDef)
  }

  val irBytes: Array[Byte] = {
    val stream = new ByteArrayOutputStream
    try ir.Serializers.serialize(stream, TheClassDef)
    finally stream.close()
    stream.toByteArray
  }
}
