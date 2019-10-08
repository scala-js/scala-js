package build

/*
 * Hard-coded IR for java.lang.Object.
 */

import java.io.ByteArrayOutputStream

import org.scalajs.ir
import ir._
import ir.Definitions._
import ir.Trees._
import ir.Types._
import ir.Position.NoPosition

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
    val ThisType = ClassType(ObjectClass)

    val EAF = ApplyFlags.empty

    val classDef = ClassDef(
      ClassIdent(ObjectClass, Some("java.lang.Object")),
      ClassKind.Class,
      None,
      None,
      Nil,
      None,
      None,
      List(
        /* def this() = () */
        MethodDef(
          MemberFlags.empty.withNamespace(MemberNamespace.Constructor),
          MethodIdent(NoArgConstructorName, Some("<init>")),
          Nil,
          NoType,
          Some(Skip()))(OptimizerHints.empty, None),

        /* def getClass(): java.lang.Class[_] = <getclass>(this) */
        MethodDef(
          MemberFlags.empty,
          MethodIdent(MethodName("getClass__jl_Class"), Some("getClass__jl_Class")),
          Nil,
          ClassType(ClassClass),
          Some {
            GetClass(This()(ThisType))
          })(OptimizerHints.empty.withInline(true), None),

        /* def hashCode(): Int = System.identityHashCode(this) */
        MethodDef(
          MemberFlags.empty,
          MethodIdent(MethodName("hashCode__I"), Some("hashCode__I")),
          Nil,
          IntType,
          Some {
            Apply(
              EAF,
              LoadModule(ClassRef(ClassName("jl_System$"))),
              MethodIdent(MethodName("identityHashCode__O__I"), Some("identityHashCode")),
              List(This()(ThisType)))(IntType)
          })(OptimizerHints.empty, None),

        /* def equals(that: Object): Boolean = this eq that */
        MethodDef(
          MemberFlags.empty,
          MethodIdent(MethodName("equals__O__Z"), Some("equals__O__Z")),
          List(ParamDef(LocalIdent(LocalName("that"), Some("that")), AnyType,
            mutable = false, rest = false)),
          BooleanType,
          Some {
            BinaryOp(BinaryOp.===,
              This()(ThisType),
              VarRef(LocalIdent(LocalName("that"), Some("that")))(AnyType))
          })(OptimizerHints.empty.withInline(true), None),

        /* protected def clone(): Object =
         *   if (this.isInstanceOf[Cloneable]) <clone>(this)
         *   else throw new CloneNotSupportedException()
         */
        MethodDef(
          MemberFlags.empty,
          MethodIdent(MethodName("clone__O"), Some("clone__O")),
          Nil,
          AnyType,
          Some {
            If(IsInstanceOf(This()(ThisType), ClassType(ClassName("jl_Cloneable"))), {
              Apply(EAF, LoadModule(ClassRef(ClassName("jl_ObjectClone$"))),
                  MethodIdent(MethodName("clone__O__O"), Some("clone")),
                  List(This()(ThisType)))(AnyType)
            }, {
              Throw(New(ClassRef(ClassName("jl_CloneNotSupportedException")),
                MethodIdent(MethodName("init___"), Some("<init>")), Nil))
            })(AnyType)
          })(OptimizerHints.empty.withInline(true), None),

        /* def toString(): String =
         *   getClass().getName() + "@" + Integer.toHexString(hashCode())
         */
        MethodDef(
          MemberFlags.empty,
          MethodIdent(MethodName("toString__T"), Some("toString__T")),
          Nil,
          ClassType(BoxedStringClass),
          Some {
            BinaryOp(BinaryOp.String_+, BinaryOp(BinaryOp.String_+,
              Apply(
                EAF,
                Apply(EAF, This()(ThisType),
                  MethodIdent(MethodName("getClass__jl_Class"), Some("getClass__jl_Class")), Nil)(
                  ClassType(ClassClass)),
                MethodIdent(MethodName("getName__T")), Nil)(ClassType(BoxedStringClass)),
              // +
              StringLiteral("@")),
              // +
              Apply(
                EAF,
                LoadModule(ClassRef(ClassName("jl_Integer$"))),
                MethodIdent(MethodName("toHexString__I__T")),
                List(Apply(EAF, This()(ThisType), MethodIdent(MethodName("hashCode__I")), Nil)(IntType)))(
                ClassType(BoxedStringClass)))
          })(OptimizerHints.empty, None),

        /* Since wait() is not supported in any way, a correct implementation
         * of notify() and notifyAll() is to do nothing.
         */

        /* def notify(): Unit = () */
        MethodDef(
          MemberFlags.empty,
          MethodIdent(MethodName("notify__V"), Some("notify__V")),
          Nil,
          NoType,
          Some(Skip()))(OptimizerHints.empty, None),

        /* def notifyAll(): Unit = () */
        MethodDef(
          MemberFlags.empty,
          MethodIdent(MethodName("notifyAll__V"), Some("notifyAll__V")),
          Nil,
          NoType,
          Some(Skip()))(OptimizerHints.empty, None),

        /* def finalize(): Unit = () */
        MethodDef(
          MemberFlags.empty,
          MethodIdent(MethodName("finalize__V"), Some("finalize__V")),
          Nil,
          NoType,
          Some(Skip()))(OptimizerHints.empty, None),

        // Exports

        /* JSExport for toString(). */
        JSMethodDef(
          MemberFlags.empty,
          StringLiteral("toString"),
          Nil,
          {
            Apply(EAF, This()(ThisType),
                MethodIdent(MethodName("toString__T"), Some("toString__T")),
                Nil)(ClassType(BoxedStringClass))
          })(OptimizerHints.empty, None)
      ),
      Nil)(OptimizerHints.empty)

    Hashers.hashClassDef(classDef)
  }

  val irBytes: Array[Byte] = {
    val stream = new ByteArrayOutputStream
    try ir.Serializers.serialize(stream, TheClassDef)
    finally stream.close()
    stream.toByteArray
  }
}
