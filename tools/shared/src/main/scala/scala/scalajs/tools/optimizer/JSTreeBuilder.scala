package scala.scalajs.tools.optimizer

import scala.scalajs.ir
import scala.scalajs.tools.javascript

/** An abstract builder taking IR or JSTrees */
trait JSTreeBuilder {
  /** Add a JavaScript tree representing a statement.
   *  The tree must be a valid JavaScript tree (typically obtained by
   *  desugaring a full-fledged IR tree).
   */
  def addJSTree(tree: javascript.Trees.Tree): Unit

  /** Completes the builder. */
  def complete(): Unit = ()
}
