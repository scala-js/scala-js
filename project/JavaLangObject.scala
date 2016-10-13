/*
 * Hard-coded IR for java.lang.Object.
 */

import org.scalajs.core.ir
import ir._
import ir.Definitions._
import ir.Infos._
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

  val InfoAndTree = {
    implicit val DummyPos = NoPosition

    // ClassType(Object) is normally invalid, but not in this class def
    val ThisType = ClassType(ObjectClass)

    val classDef = ClassDef(
      Ident("O", Some("java.lang.Object")),
      ClassKind.Class,
      None,
      Nil,
      None,
      List(
        /* def this() = () */
        MethodDef(
          static = false,
          Ident("init___", Some("<init>")),
          Nil,
          NoType,
          Some(Skip()))(OptimizerHints.empty, None),

        /* def getClass(): java.lang.Class[_] = <getclass>(this) */
        MethodDef(
          static = false,
          Ident("getClass__jl_Class", Some("getClass__jl_Class")),
          Nil,
          ClassType(ClassClass),
          Some {
            GetClass(This()(ThisType))
          })(OptimizerHints.empty.withInline(true), None),

        /* def hashCode(): Int = System.identityHashCode(this) */
        MethodDef(
          static = false,
          Ident("hashCode__I", Some("hashCode__I")),
          Nil,
          IntType,
          Some {
            Apply(
              LoadModule(ClassType("jl_System$")),
              Ident("identityHashCode__O__I", Some("identityHashCode")),
              List(This()(ThisType)))(IntType)
          })(OptimizerHints.empty, None),

        /* def equals(that: Object): Boolean = this eq that */
        MethodDef(
          static = false,
          Ident("equals__O__Z", Some("equals__O__Z")),
          List(ParamDef(Ident("that", Some("that")), AnyType,
            mutable = false, rest = false)),
          BooleanType,
          Some {
            BinaryOp(BinaryOp.===,
              This()(ThisType),
              VarRef(Ident("that", Some("that")))(AnyType))
          })(OptimizerHints.empty.withInline(true), None),

        /* protected def clone(): Object =
         *   if (this.isInstanceOf[Cloneable]) <clone>(this)
         *   else throw new CloneNotSupportedException()
         */
        MethodDef(
          static = false,
          Ident("clone__O", Some("clone__O")),
          Nil,
          AnyType,
          Some {
            If(IsInstanceOf(This()(ThisType), ClassType("jl_Cloneable")), {
              Apply(LoadModule(ClassType("sjsr_package$")),
                  Ident("cloneObject__sjs_js_Object__sjs_js_Object", Some("cloneObject")),
                  List(This()(ThisType)))(AnyType)
            }, {
              Throw(New(ClassType("jl_CloneNotSupportedException"),
                Ident("init___", Some("<init>")), Nil))
            })(AnyType)
          })(OptimizerHints.empty.withInline(true), None),

        /* def toString(): String =
         *   getClass().getName() + "@" + Integer.toHexString(hashCode())
         */
        MethodDef(
          static = false,
          Ident("toString__T", Some("toString__T")),
          Nil,
          ClassType(StringClass),
          Some {
            BinaryOp(BinaryOp.String_+, BinaryOp(BinaryOp.String_+,
              Apply(
                Apply(This()(ThisType),
                  Ident("getClass__jl_Class", Some("getClass__jl_Class")), Nil)(
                  ClassType(ClassClass)),
                Ident("getName__T"), Nil)(ClassType(StringClass)),
              // +
              StringLiteral("@")),
              // +
              Apply(
                LoadModule(ClassType("jl_Integer$")),
                Ident("toHexString__I__T"),
                List(Apply(This()(ThisType), Ident("hashCode__I"), Nil)(IntType)))(
                ClassType(StringClass)))
          })(OptimizerHints.empty, None),

        /* Since wait() is not supported in any way, a correct implementation
         * of notify() and notifyAll() is to do nothing.
         */

        /* def notify(): Unit = () */
        MethodDef(
          static = false,
          Ident("notify__V", Some("notify__V")),
          Nil,
          NoType,
          Some(Skip()))(OptimizerHints.empty, None),

        /* def notifyAll(): Unit = () */
        MethodDef(
          static = false,
          Ident("notifyAll__V", Some("notifyAll__V")),
          Nil,
          NoType,
          Some(Skip()))(OptimizerHints.empty, None),

        /* def finalize(): Unit = () */
        MethodDef(
          static = false,
          Ident("finalize__V", Some("finalize__V")),
          Nil,
          NoType,
          Some(Skip()))(OptimizerHints.empty, None),

        // Exports

        /* JSExport for toString(). */
        MethodDef(
          static = false,
          StringLiteral("toString"),
          Nil,
          AnyType,
          Some {
            Apply(This()(ThisType),
                Ident("toString__T", Some("toString__T")),
                Nil)(ClassType(StringClass))
          })(OptimizerHints.empty, None)
      ))(OptimizerHints.empty)

    val hashedClassedDef = Hashers.hashClassDef(classDef)
    val info = generateClassInfo(hashedClassedDef)

    (info, hashedClassedDef)
  }

}
