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

package org.scalajs.nscplugin.test.util

import language.implicitConversions

import scala.tools.nsc._
import scala.reflect.internal.util.SourceFile

import scala.util.control.ControlThrowable

import org.junit.Assert._

import org.scalajs.nscplugin.ScalaJSPlugin
import org.scalajs.ir
import ir.{Trees => js}

abstract class JSASTTest extends DirectTest {

  private var lastAST: JSAST = _

  class JSAST(val clDefs: List[js.ClassDef]) {
    type Pat = PartialFunction[js.IRNode, Unit]

    class PFTraverser(pf: Pat) extends ir.Traversers.Traverser {
      private case object Found extends ControlThrowable

      private[this] var finding = false

      def find: Boolean = {
        finding = true
        try {
          clDefs.map(traverseClassDef)
          false
        } catch {
          case Found => true
        }
      }

      def traverse(): Unit = {
        finding = false
        clDefs.map(traverseClassDef)
      }

      override def traverse(tree: js.Tree): Unit = {
        handle(tree)
        super.traverse(tree)
      }

      override def traverseClassDef(classDef: js.ClassDef): Unit = {
        handle(classDef)
        super.traverseClassDef(classDef)
      }

      override def traverseAnyFieldDef(fieldDef: js.AnyFieldDef): Unit = {
        handle(fieldDef)
        super.traverseAnyFieldDef(fieldDef)
      }

      override def traverseMethodDef(methodDef: js.MethodDef): Unit = {
        handle(methodDef)
        super.traverseMethodDef(methodDef)
      }

      override def traverseJSConstructorDef(jsConstructor: js.JSConstructorDef): Unit = {
        handle(jsConstructor)
        super.traverseJSConstructorDef(jsConstructor)
      }

      override def traverseJSMethodPropDef(jsMethodPropDef: js.JSMethodPropDef): Unit = {
        handle(jsMethodPropDef)
        super.traverseJSMethodPropDef(jsMethodPropDef)
      }

      override def traverseTopLevelExportDef(
          exportDef: js.TopLevelExportDef): Unit = {
        handle(exportDef)
        super.traverseTopLevelExportDef(exportDef)
      }

      private def handle(node: js.IRNode): Unit = {
        if (finding) {
          if (pf.isDefinedAt(node))
            throw Found
        } else {
          pf.lift(node)
        }
      }
    }

    def has(trgName: String)(pf: Pat): this.type = {
      val tr = new PFTraverser(pf)
      if (!tr.find)
        fail(s"AST should have $trgName but was\n$show")
      this
    }

    def hasNot(trgName: String)(pf: Pat): this.type = {
      val tr = new PFTraverser(pf)
      if (tr.find)
        fail(s"AST should not have $trgName but was\n$show")
      this
    }

    def hasExactly(count: Int, trgName: String)(pf: Pat): this.type = {
      var actualCount = 0
      val tr = new PFTraverser(pf.andThen(_ => actualCount += 1))
      tr.traverse()
      if (actualCount != count)
        fail(s"AST has $actualCount $trgName but expected $count; it was\n$show")
      this
    }

    def extractOne[A](trgName: String)(pf: PartialFunction[js.IRNode, A]): A = {
      var result: Option[A] = None
      val tr = new PFTraverser(pf.andThen { r =>
        if (result.isDefined)
          fail(s"AST has more than one $trgName")
        result = Some(r)
      })
      tr.traverse()
      result.getOrElse {
        fail(s"AST should have a $trgName")
        throw new AssertionError("unreachable")
      }
    }

    def traverse(pf: Pat): this.type = {
      val tr = new PFTraverser(pf)
      tr.traverse()
      this
    }

    def show: String =
      clDefs.map(_.show).mkString("\n")

  }

  implicit def string2ast(str: String): JSAST = stringAST(str)

  override def newScalaJSPlugin(global: Global): ScalaJSPlugin = {
    new ScalaJSPlugin(global) {
      override def generatedJSAST(cld: List[js.ClassDef]): Unit = {
        lastAST = new JSAST(cld)
      }
    }
  }

  def stringAST(code: String): JSAST = stringAST(defaultGlobal)(code)
  def stringAST(global: Global)(code: String): JSAST = {
    if (!compileString(global)(code))
      throw new IllegalArgumentException("snippet did not compile")
    lastAST
  }

  def sourceAST(source: SourceFile): JSAST = sourceAST(defaultGlobal)(source)
  def sourceAST(global: Global)(source: SourceFile): JSAST = {
    if (!compileSources(global)(source))
      throw new IllegalArgumentException("snippet did not compile")
    lastAST
  }

}
