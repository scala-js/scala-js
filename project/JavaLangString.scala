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

  /** Optimizer hints with `@inline`
   *  Unfortunately we do not have access to private details of
   *  [[OptimizerHints]], so we cannot do this cleanly. But it is fine
   *  somehow because we're part of the same project implementation.
   */
  private def inlineOptimizerHints = new OptimizerHints(1)

  val InfoAndTree = (Info, Definition)

  private def Info = ClassInfo(
    name = "java.lang.String",
    encodedName = "T",
    ancestorCount = 1,
    kind = ClassKind.HijackedClass,
    superClass = "O",
    ancestors = List(
      "T", "Ljava_io_Serializable", "jl_CharSequence", "jl_Comparable", "O"),
    methods = List(
      MethodInfo("equals__O__Z",
        optimizerHints = inlineOptimizerHints
      ),
      MethodInfo("hashCode__I",
        calledMethods = Map(
          "sjsr_RuntimeString$" -> List("hashCode__T__I")
        ),
        accessedModules = List("sjsr_RuntimeString"),
        optimizerHints = inlineOptimizerHints
      ),
      MethodInfo("compareTo__T__I",
        calledMethods = Map(
          "sjsr_RuntimeString$" -> List("compareTo__T__T__I")
        ),
        accessedModules = List("sjsr_RuntimeString"),
        optimizerHints = inlineOptimizerHints
      ),
      MethodInfo("compareTo__O__I",
        calledMethods = Map(
          "T" -> List("compareTo__T__I")
        ),
        optimizerHints = inlineOptimizerHints
      ),
      MethodInfo("toString__T",
        optimizerHints = inlineOptimizerHints
      ),
      MethodInfo("charAt__I__C",
        calledMethods = Map(
          "sjsr_RuntimeString$" -> List("charAt__T__I__C")
        ),
        accessedModules = List("sjsr_RuntimeString"),
        optimizerHints = inlineOptimizerHints
      ),
      MethodInfo("length__I",
        calledMethods = Map(
          "sjsr_RuntimeString$" -> List("length__T__I")
        ),
        accessedModules = List("sjsr_RuntimeString"),
        optimizerHints = inlineOptimizerHints
      ),
      MethodInfo("subSequence__I__I__jl_CharSequence",
        calledMethods = Map(
          "sjsr_RuntimeString$" -> List("subSequence__T__I__I__jl_CharSequence")
        ),
        accessedModules = List("sjsr_RuntimeString"),
        optimizerHints = inlineOptimizerHints
      )
    )
  )

  private def Definition = {
    implicit val DummyPos = NoPosition

    val ThisType = ClassType(StringClass)

    ClassDef(
      Ident("T", Some("java.lang.String")),
      ClassKind.HijackedClass,
      Some(Ident("O", Some("java.lang.Object"))),
      List(
          Ident("Ljava_io_Serializable", Some("java.io.Serializable")),
          Ident("jl_CharSequence", Some("java.lang.CharSequence")),
          Ident("jl_Comparable", Some("java.lang.Comparable")),
          Ident("O", Some("java.lang.Object"))
      ),
      List(
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

        /* def hashCode(): Int = RuntimeString.hashCode(this) */
        MethodDef(
          Ident("hashCode__I", Some("hashCode__I")),
          Nil,
          IntType,
          {
            Apply(
              LoadModule(ClassType("sjsr_RuntimeString$")),
              Ident("hashCode__T__I", Some("hashCode__T__I")),
              List(This()(ThisType)))(IntType)
          })(None),

        /* def compareTo(that: String): Int = RuntimeString.compareTo(this, that) */
        MethodDef(
          Ident("compareTo__T__I", Some("compareTo__T__I")),
          List(ParamDef(Ident("that", Some("that")), ThisType, mutable = false)),
          IntType,
          {
            Apply(
              LoadModule(ClassType("sjsr_RuntimeString$")),
              Ident("compareTo__T__T__I", Some("compareTo__T__T__I")),
              List(
                This()(ThisType),
                VarRef(Ident("that", Some("that")), mutable = false)(ThisType)))(IntType)
          })(None),

        /* def compareTo(that: Object): Int = compareTo(that.asInstanceOf[String]) */
        MethodDef(
          Ident("compareTo__O__I", Some("compareTo__O__I")),
          List(ParamDef(Ident("that", Some("that")), AnyType, mutable = false)),
          IntType,
          {
            Apply(
              This()(ThisType),
              Ident("compareTo__T__I", Some("compareTo__T__I")),
              List(AsInstanceOf(
                VarRef(Ident("that", Some("that")), mutable = false)(AnyType),
                ThisType)))(IntType)
          })(None),

        /* def toString(): String = this */
        MethodDef(
          Ident("toString__T", Some("toString__T")),
          Nil,
          ClassType(StringClass),
          {
            This()(ThisType)
          })(None),

        /* def charAt(i: Int): Char = RuntimeString.charAt(this, i) */
        MethodDef(
          Ident("charAt__I__C", Some("charAt__I__C")),
          List(ParamDef(Ident("i", Some("i")), IntType, mutable = false)),
          IntType,
          {
            Apply(
              LoadModule(ClassType("sjsr_RuntimeString$")),
              Ident("charAt__T__I__C", Some("charAt__T__I__C")),
              List(
                This()(ThisType),
                VarRef(Ident("i", Some("i")), mutable = false)(IntType)))(IntType)
          })(None),

        /* def length(): Int = RuntimeString.length(this) */
        MethodDef(
          Ident("length__I", Some("length__I")),
          Nil,
          IntType,
          {
            Apply(
              LoadModule(ClassType("sjsr_RuntimeString$")),
              Ident("length__T__I", Some("length__T__I")),
              List(This()(ThisType)))(IntType)
          })(None),

        /* def subSequence(begin: Int, end: Int): CharSequence =
         *   RuntimeString.subSequence(this, begin, end)
         */
        MethodDef(
          Ident("subSequence__I__I__jl_CharSequence",
            Some("subSequence__I__I__jl_CharSequence")),
          List(
            ParamDef(Ident("begin", Some("begin")), IntType, mutable = false),
            ParamDef(Ident("end", Some("end")), IntType, mutable = false)
          ),
          ClassType("jl_CharSequence"),
          {
            Apply(
              LoadModule(ClassType("sjsr_RuntimeString$")),
              Ident("subSequence__T__I__I__jl_CharSequence",
                Some("subSequence__T__I__I__jl_CharSequence")),
              List(
                This()(ThisType),
                VarRef(Ident("begin", Some("begin")), mutable = false)(IntType),
                VarRef(Ident("end", Some("end")), mutable = false)(IntType)))(
              ClassType("jl_CharSequence"))
          })(None)
      ))
  }

}
