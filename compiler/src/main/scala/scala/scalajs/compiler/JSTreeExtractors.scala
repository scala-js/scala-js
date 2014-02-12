/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Tobias Schlatter
 */

package scala.scalajs.compiler

import scala.annotation.tailrec

/** Useful extractors for JavaScript trees */
trait JSTreeExtractors { this: JSTrees =>

  import js._

  object jse {
    /**
     *  A qualified identifier with dot select.
     *
     *  Example (JS): ScalaJS.c.foo
     */
    object QualIdent {
      def unapply(tree: Tree): Option[String] = tree match {
        case DotSelect(prefix, Ident(name, _)) =>
          unapply(prefix) map { _ + s".$name" }
        case Ident(name, _) => Some(name)
        case _ => None
      }
    }

    /**
     *  A literally named sequence (like in a call to applyDynamicNamed)
     *
     *  Example (Scala): method(("name1", x), ("name2", y))
     */
    object LitNamed {
      def unapply(exprs: List[Tree]) = unapply0(exprs, Nil)

      @tailrec
      private def unapply0(
          exprs: List[Tree],
          acc: List[(StringLiteral, Tree)]
        ): Option[List[(StringLiteral, Tree)]] = exprs match {
        case Tuple2(name: StringLiteral, value) :: xs =>
          unapply0(xs, (name, value) :: acc)
        case Nil => Some(acc.reverse)
        case _   => None
      }
    }

    /**
     *  A literal Tuple2
     *
     *  Example  (Scala): (x, y)
     *  But also (Scala): x -> y
     */
    object Tuple2 {
      def unapply(tree: Tree) = tree match {
        // case (x,y)
        case ApplyMethod(js.New(
            QualIdent("ScalaJS.c.scala_Tuple2"), Nil),
            Ident("init___O__O", _),
            List(_1,_2)) => Some((_1,_2))
        // case x -> y
        case ApplyMethod(
            ScalaObject("ScalaJS.modules.scala_Predef$ArrowAssoc"),
            Ident("$$minus$greater$extension__O__O__Lscala_Tuple2", _),
            ApplyMethod(
              ScalaObject("ScalaJS.modules.scala_Predef"),
              Ident("any2ArrowAssoc__O__O", _),
              List(_1)) :: _2 :: Nil) => Some((_1, _2))
        case _ =>
          None
      }
    }

    /**
     *  Fetching a Scala module (via it's accessor method)
     */
    object ScalaObject {
      def unapply(tree: Tree) = tree match {
        case ApplyMethod(QualIdent(id), Ident(name, _), Nil) =>
          Some(s"$id.$name")
        case _ => None
      }
    }
  }

}
