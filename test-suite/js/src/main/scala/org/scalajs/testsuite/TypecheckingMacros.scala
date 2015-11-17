/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite

import Compat210._

object TypecheckingMacros {
  import scala.reflect.macros._
  import blackbox.Context

  private class Macros[C <: Context](val c: C) extends Compat210Component {
    import c.universe._

    def typeError(code: String, expectedMsg: Option[String]): c.Expr[Unit] = {
      val error = try {
        c.typecheck(c.parse(s"{ $code }"))
        c.abort(c.enclosingPosition,
            "Expected type error, type checked successfully.")
      } catch {
        case e: TypecheckException =>
          val errMsg = e.getMessage
          for (msg <- expectedMsg) {
            if (errMsg != msg) {
              c.abort(c.enclosingPosition,
                  s"Type errors mismatch.\nExpected: $msg\nFound: $errMsg")
            }
          }
      }

      reify(())
    }

    def treeToString(t: Tree): String = t match {
      case Literal(Constant(str: String)) => str

      case Apply(Select(t1, name), List(t2)) if name.decoded == "+" =>
        treeToString(t1) + treeToString(t2)

      case _ =>
        c.abort(t.pos, "Expected literal string.")

    }
  }

  def typeError(c: Context)(code: c.Expr[String]): c.Expr[Unit] = {
    import c.universe._
    val m = new Macros[c.type](c)
    m.typeError(m.treeToString(code.tree), None)
  }

  def typeErrorWithMsg(c: Context)(code: c.Expr[String],
      msg: c.Expr[String]): c.Expr[Unit] = {
    import c.universe._
    val m = new Macros[c.type](c)
    m.typeError(m.treeToString(code.tree), Some(m.treeToString(msg.tree)))
  }

}
