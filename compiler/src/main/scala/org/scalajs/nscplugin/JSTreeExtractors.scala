/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.nscplugin

import scala.annotation.tailrec

import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

/** Useful extractors for JavaScript trees */
object JSTreeExtractors {

  object jse {

    object BlockOrAlone {
      def unapply(tree: Tree): Some[(List[Tree], Tree)] = Some(tree match {
        case Block(trees) => (trees.init, trees.last)
        case _            => (Nil, tree)
      })
    }

  }

}
