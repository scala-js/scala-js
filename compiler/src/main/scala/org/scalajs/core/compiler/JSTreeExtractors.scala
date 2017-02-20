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

  object jse { // scalastyle:ignore

    object BlockOrAlone {
      def unapply(tree: Tree): Some[(List[Tree], Tree)] = Some(tree match {
        case Block(trees) => (trees.init, trees.last)
        case _            => (Nil, tree)
      })
    }

    /** Extracts the literal strings in "key" position of a sequence of Tuple2.
     *
     *  Non-Tuple2 constructors are silently ignored, as well as non-literal
     *  keys.
     */
    def extractLiteralKeysFrom(exprs: List[Tree]): List[StringLiteral] = {
      exprs.collect {
        case Tuple2(key: StringLiteral, _) => key
      }
    }

    /** A list of Tuple2, for example used as a list of key/value pairs
     *  (like in a call to applyDynamicNamed).
     *
     *  Examples (Scala):
     *  {{{
     *  method(("name1", x), ("name2", y))
     *  method("name1" -> x, "name2" -> y)
     *  method(nameExpr1 -> x, (nameExpr2, y))
     *  }}}
     */
    object Tuple2List {
      def unapply(exprs: List[Tree]): Option[List[(Tree, Tree)]] = {
        val tuples = exprs.collect {
          case Tuple2(key, value) => (key, value)
        }
        if (tuples.size == exprs.size)
          Some(tuples)
        else
          None
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
                Ident("any2ArrowAssoc__O__O" | "ArrowAssoc__O__O", _),
                List(_1)),
              _2)) =>
          Some((_1, _2))
        case _ =>
          None
      }
    }
  }

}
