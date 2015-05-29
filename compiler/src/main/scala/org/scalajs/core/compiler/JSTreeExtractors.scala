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

    /**
     *  A partially literally named sequence (like in a call to applyDynamicNamed)
     *  Where some parameters are expected to be literally named.
     *
     *  Example (Scala): method(("name1", x), (a, y), z)
     */
    object LitNamedExtractor {
      def extractFrom(exprs: List[Tree]): List[(StringLiteral, Tree)] = {
        // Note that with 'failIfNonLit = false'
        // genNameLitExtract will never return None
        genNamedLitExtract(exprs, Nil, false).getOrElse(Nil)
      }

      @tailrec
      private[jse] final def genNamedLitExtract(
          exprs: List[Tree],
          acc: List[(StringLiteral, Tree)],
          failIfNonLit: Boolean
        ): Option[List[(StringLiteral, Tree)]] = exprs match {
        case Tuple2(name: StringLiteral, value) :: xs =>
          genNamedLitExtract(xs, (name, value) :: acc, failIfNonLit)
        case _ :: xs =>
          if (failIfNonLit)
            None
          else
            genNamedLitExtract(xs, acc, failIfNonLit)
        case Nil => Some(acc.reverse)
      }
    }

    /**
     *  A literally named sequence (like in a call to applyDynamicNamed)
     *  Where all parameters are expected to be literally named.
     *
     *  Example (Scala): method(("name1", x), ("name2", y))
     */
    object LitNamed {
      def unapply(exprs: List[Tree]): Option[List[(StringLiteral, Tree)]] = {
        LitNamedExtractor.genNamedLitExtract(exprs, Nil, true)
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
