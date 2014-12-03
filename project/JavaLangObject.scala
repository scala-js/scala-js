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

  /** Optimizer hints with `@inline` */
  private def inlineOptimizerHints =
    OptimizerHints.empty.withHasInlineAnnot(true)

  val InfoAndTree = (Info, Definition)

  private def Info = ClassInfo(
    name = "java.lang.Object",
    encodedName = "O",
    ancestorCount = 0,
    kind = ClassKind.Class,
    superClass = "",
    ancestors = List("O"),
    methods = List(
      MethodInfo("__init__"),
      MethodInfo("init___"),
      MethodInfo("hashCode__I",
        calledMethods = Map(
          "jl_System$" -> List("identityHashCode__O__I")
        ),
        accessedModules = List("jl_System")
      ),
      MethodInfo("equals__O__Z",
        optimizerHints = inlineOptimizerHints
      ),
      MethodInfo("clone__O",
        calledMethods = Map(
          "sjsr_package$" -> List("cloneObject__sjs_js_Object__sjs_js_Object"),
          "jl_CloneNotSupportedException" -> List("init___")
        ),
        instantiatedClasses = List("jl_CloneNotSupportedException"),
        accessedModules = List("sjsr_package"),
        accessedClassData = List("jl_Cloneable"),
        optimizerHints = inlineOptimizerHints
      ),
      MethodInfo("notify__V"),
      MethodInfo("notifyAll__V"),
      MethodInfo("toString__T",
        calledMethods = Map(
          "O" -> List("hashCode__I"),
          "jl_Class" -> List("getName__T"),
          "jl_Integer$" -> List("toHexString__I__T")
        ),
        accessedModules = List("jl_Integer")
      ),
      MethodInfo("finalize__V"),
      MethodInfo("clone__",
        calledMethods = Map(
          "O" -> List("clone__O")
        )
      ),
      MethodInfo("notify__",
        calledMethods = Map(
          "O" -> List("notify__V")
        )
      ),
      MethodInfo("notifyAll__",
        calledMethods = Map(
          "O" -> List("notifyAll__V")
        )
      ),
      MethodInfo("finalize__",
        calledMethods = Map(
          "O" -> List("finalize__V")
        )
      )
    )
  )

  private def Definition = {
    implicit val DummyPos = NoPosition

    // ClassType(Object) is normally invalid, but not in this class def
    val ThisType = ClassType(ObjectClass)

    val classDef = ClassDef(
      Ident("O", Some("java.lang.Object")),
      ClassKind.Class,
      None,
      Nil,
      List(
        /* def this() = () */
        MethodDef(
          Ident("init___", Some("<init>")),
          Nil,
          AnyType,
          This()(ThisType))(None),

        /* def hashCode(): Int = System.identityHashCode(this) */
        MethodDef(
          Ident("hashCode__I", Some("hashCode__I")),
          Nil,
          IntType,
          {
            Apply(
              LoadModule(ClassType("jl_System$")),
              Ident("identityHashCode__O__I", Some("identityHashCode")),
              List(This()(ThisType)))(IntType)
          })(None),

        /* def equals(that: Object): Boolean = this eq that */
        MethodDef(
          Ident("equals__O__Z", Some("equals__O__Z")),
          List(ParamDef(Ident("that", Some("that")), AnyType, mutable = false)),
          BooleanType,
          {
            BinaryOp(BinaryOp.===,
              This()(ThisType),
              VarRef(Ident("that", Some("that")), mutable = false)(AnyType))
          })(None),

        /* protected def clone(): Object =
         *   if (this.isInstanceOf[Cloneable]) <clone>(this)
         *   else throw new CloneNotSupportedException()
         */
        MethodDef(
          Ident("clone__O", Some("clone__O")),
          Nil,
          AnyType,
          {
            If(IsInstanceOf(This()(ThisType), ClassType("jl_Cloneable")), {
              Apply(LoadModule(ClassType("sjsr_package$")),
                  Ident("cloneObject__sjs_js_Object__sjs_js_Object", Some("cloneObject")),
                  List(This()(ThisType)))(AnyType)
            }, {
              Throw(New(ClassType("jl_CloneNotSupportedException"),
                Ident("init___", Some("<init>")), Nil))
            })(AnyType)
          })(None),

        /* def toString(): String =
         *   getClass().getName() + "@" + Integer.toHexString(hashCode())
         */
        MethodDef(
          Ident("toString__T", Some("toString__T")),
          Nil,
          ClassType(StringClass),
          {
            BinaryOp(BinaryOp.String_+, BinaryOp(BinaryOp.String_+,
              Apply(
                GetClass(This()(ThisType)),
                Ident("getName__T"), Nil)(ClassType(StringClass)),
              // +
              StringLiteral("@")),
              // +
              Apply(
                LoadModule(ClassType("jl_Integer$")),
                Ident("toHexString__I__T"),
                List(Apply(This()(ThisType), Ident("hashCode__I"), Nil)(IntType)))(
                ClassType(StringClass)))
          })(None),

          /* Since wait() is not supported in any way, a correct implementation
           * of notify() and notifyAll() is to do nothing.
           */

          /* def notify(): Unit = () */
          MethodDef(
            Ident("notify__V", Some("notify__V")),
            Nil,
            NoType,
            Skip())(None),

          /* def notifyAll(): Unit = () */
          MethodDef(
            Ident("notifyAll__V", Some("notifyAll__V")),
            Nil,
            NoType,
            Skip())(None),

          /* def finalize(): Unit = () */
          MethodDef(
            Ident("finalize__V", Some("finalize__V")),
            Nil,
            NoType,
            Skip())(None),

          /* Reflective proxies
           * Note that we do not need to proxy the following methods, since
           * they are defined on Any in the Scala hierarchy and therefore a
           * reflective call is never emitted:
           * - equals
           * - getClass
           * - hashCode
           * - toString
           */

          MethodDef(Ident("clone__"), Nil, AnyType,
            Apply(This()(ThisType), Ident("clone__O"), Nil)(AnyType))(None),

          MethodDef(Ident("notify__"), Nil, AnyType, Block(
            Apply(This()(ThisType), Ident("notify__V"), Nil)(NoType),
            Undefined()))(None),

          MethodDef(Ident("notifyAll__"), Nil, AnyType, Block(
            Apply(This()(ThisType), Ident("notifyAll__V"), Nil)(NoType),
            Undefined()))(None),

          MethodDef(Ident("finalize__"), Nil, AnyType, Block(
            Apply(This()(ThisType), Ident("finalize__V"), Nil)(NoType),
            Undefined()))(None),

          // Exports

          /* JSExport for toString(). */
          MethodDef(
            StringLiteral("toString"),
            Nil,
            AnyType,
            {
              Apply(This()(ThisType),
                  Ident("toString__T", Some("toString__T")),
                  Nil)(ClassType(StringClass))
            })(None)
      ))

      Hashers.hashClassDef(classDef)
  }

}
