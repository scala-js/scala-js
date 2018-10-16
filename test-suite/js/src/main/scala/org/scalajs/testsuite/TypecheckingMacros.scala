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

package org.scalajs.testsuite

import scala.reflect.macros.TypecheckException
import scala.reflect.macros.blackbox.Context

object TypecheckingMacros {
  private class Macros[C <: Context](val c: C) {
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
