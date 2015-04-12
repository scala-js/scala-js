/*
 * Hard-coded IR for java.lang.String.
 */

import org.scalajs.core.ir
import ir._
import ir.Definitions._
import ir.Infos._
import ir.Trees._
import ir.Types._
import ir.Position.NoPosition

/** Hard-coded IR for java.lang.String.
 *  Unlike for the other hijacked classes, scalac does not like at all to
 *  compile even a mocked version of java.lang.String. So we have to bypass
 *  entirely the compiler to define java.lang.String.
 */
object JavaLangString {

  val InfoAndTree = {
    implicit val DummyPos = NoPosition

    val ThisType = ClassType(StringClass)

    val classDef = ClassDef(
      Ident("T", Some("java.lang.String")),
      ClassKind.HijackedClass,
      Some(Ident("O", Some("java.lang.Object"))),
      List(
          Ident("Ljava_io_Serializable", Some("java.io.Serializable")),
          Ident("jl_CharSequence", Some("java.lang.CharSequence")),
          Ident("jl_Comparable", Some("java.lang.Comparable"))
      ),
      None,
      List(
        /* def equals(that: Object): Boolean = this eq that */
        MethodDef(
          static = false,
          Ident("equals__O__Z", Some("equals__O__Z")),
          List(ParamDef(Ident("that", Some("that")), AnyType,
            mutable = false, rest = false)),
          BooleanType,
          {
            BinaryOp(BinaryOp.===,
              This()(ThisType),
              VarRef(Ident("that", Some("that")))(AnyType))
          })(OptimizerHints.empty.withInline(true), None),

        /* def hashCode(): Int = RuntimeString.hashCode(this) */
        MethodDef(
          static = false,
          Ident("hashCode__I", Some("hashCode__I")),
          Nil,
          IntType,
          {
            Apply(
              LoadModule(ClassType("sjsr_RuntimeString$")),
              Ident("hashCode__T__I", Some("hashCode__T__I")),
              List(This()(ThisType)))(IntType)
          })(OptimizerHints.empty.withInline(true), None),

        /* def compareTo(that: String): Int = RuntimeString.compareTo(this, that) */
        MethodDef(
          static = false,
          Ident("compareTo__T__I", Some("compareTo__T__I")),
          List(ParamDef(Ident("that", Some("that")), ThisType,
            mutable = false, rest = false)),
          IntType,
          {
            Apply(
              LoadModule(ClassType("sjsr_RuntimeString$")),
              Ident("compareTo__T__T__I", Some("compareTo__T__T__I")),
              List(
                This()(ThisType),
                VarRef(Ident("that", Some("that")))(ThisType)))(IntType)
          })(OptimizerHints.empty.withInline(true), None),

        /* def compareTo(that: Object): Int = compareTo(that.asInstanceOf[String]) */
        MethodDef(
          static = false,
          Ident("compareTo__O__I", Some("compareTo__O__I")),
          List(ParamDef(Ident("that", Some("that")), AnyType,
            mutable = false, rest = false)),
          IntType,
          {
            Apply(
              This()(ThisType),
              Ident("compareTo__T__I", Some("compareTo__T__I")),
              List(AsInstanceOf(
                VarRef(Ident("that", Some("that")))(AnyType),
                ThisType)))(IntType)
          })(OptimizerHints.empty.withInline(true), None),

        /* def toString(): String = this */
        MethodDef(
          static = false,
          Ident("toString__T", Some("toString__T")),
          Nil,
          ClassType(StringClass),
          {
            This()(ThisType)
          })(OptimizerHints.empty.withInline(true), None),

        /* def charAt(i: Int): Char = RuntimeString.charAt(this, i) */
        MethodDef(
          static = false,
          Ident("charAt__I__C", Some("charAt__I__C")),
          List(ParamDef(Ident("i", Some("i")), IntType,
            mutable = false, rest = false)),
          IntType,
          {
            Apply(
              LoadModule(ClassType("sjsr_RuntimeString$")),
              Ident("charAt__T__I__C", Some("charAt__T__I__C")),
              List(
                This()(ThisType),
                VarRef(Ident("i", Some("i")))(IntType)))(IntType)
          })(OptimizerHints.empty.withInline(true), None),

        /* def length(): Int = RuntimeString.length(this) */
        MethodDef(
          static = false,
          Ident("length__I", Some("length__I")),
          Nil,
          IntType,
          {
            Apply(
              LoadModule(ClassType("sjsr_RuntimeString$")),
              Ident("length__T__I", Some("length__T__I")),
              List(This()(ThisType)))(IntType)
          })(OptimizerHints.empty.withInline(true), None),

        /* def subSequence(begin: Int, end: Int): CharSequence =
         *   RuntimeString.subSequence(this, begin, end)
         */
        MethodDef(
          static = false,
          Ident("subSequence__I__I__jl_CharSequence",
            Some("subSequence__I__I__jl_CharSequence")),
          List(
            ParamDef(Ident("begin", Some("begin")), IntType,
              mutable = false, rest = false),
            ParamDef(Ident("end", Some("end")), IntType,
              mutable = false, rest = false)
          ),
          ClassType("jl_CharSequence"),
          {
            Apply(
              LoadModule(ClassType("sjsr_RuntimeString$")),
              Ident("subSequence__T__I__I__jl_CharSequence",
                Some("subSequence__T__I__I__jl_CharSequence")),
              List(
                This()(ThisType),
                VarRef(Ident("begin", Some("begin")))(IntType),
                VarRef(Ident("end", Some("end")))(IntType)))(
              ClassType("jl_CharSequence"))
          })(OptimizerHints.empty.withInline(true), None)
      ))(OptimizerHints.empty)

    val hashedClassDef = Hashers.hashClassDef(classDef)
    val info = generateClassInfo(hashedClassDef)

    (info, hashedClassDef)
  }

}
