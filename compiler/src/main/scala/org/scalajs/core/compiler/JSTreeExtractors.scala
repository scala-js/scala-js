/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Tobias Schlatter
 */

package org.scalajs.core.compiler

import scala.annotation.tailrec

import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types._

/** Useful extractors for JavaScript trees */
object JSTreeExtractors {

  object jse {
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
      def unapply(tree: Tree): Option[(Tree, Tree)] = tree match {
        // case (x, y)
        case New(ClassType("T2"), Ident("init___O__O", _),
            List(_1, _2)) =>
          Some((_1, _2))
        // case x -> y
        case Apply(
            LoadModule(ClassType("s_Predef$ArrowAssoc$")),
            Ident("$$minus$greater$extension__O__O__T2", _),
            List(
              Apply(
                LoadModule(ClassType("s_Predef$")),
                Ident("any2ArrowAssoc__O__O", _),
                List(_1)),
              _2)) =>
          Some((_1, _2))
        case _ =>
          None
      }
    }
  }

}
