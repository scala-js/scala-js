/*
 * Hard-coded IR for java.lang.String.
 */

import scala.scalajs.ir
import ir._
import ir.Infos._
import ir.Trees._
import ir.Position.NoPosition

/** Hard-coded IR for java.lang.String.
 *  Unlike for the other hijacked classes, scalac does not like at all to
 *  compile even a mocked version of java.lang.String. So we have to bypass
 *  entirely the compiler to define java.lang.String.
 */
object JavaLangString {

  val InfoAndTree = (Info, Definition)

  private def Info = ClassInfo(
    name = "java.lang.String",
    encodedName = "T",
    ancestorCount = 1,
    kind = ClassKind.HijackedClass,
    superClass = "O",
    ancestors = List(
      "T", "Ljava_io_Serializable", "jl_CharSequence", "jl_Comparable", "O")
  )

  private def Definition = {
    implicit val DummyPos = NoPosition
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
        Nil)
  }

}
