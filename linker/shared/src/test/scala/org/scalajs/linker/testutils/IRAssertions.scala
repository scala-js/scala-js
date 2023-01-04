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

package org.scalajs.linker.testutils

import scala.language.implicitConversions

import scala.util.control.ControlThrowable

import org.scalajs.ir.ClassKind
import org.scalajs.ir.EntryPointsInfo
import org.scalajs.ir.Names._
import org.scalajs.ir.Traversers._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.linker.standard._

import org.junit.Assert._

object IRAssertions {
  implicit def classDefAssertions(classDef: ClassDef): ClassDefAssertions =
    new ClassDefAssertions(classDef)

  implicit def linkedClassAssertions(linkedClass: LinkedClass): LinkedClassAssertions =
    new LinkedClassAssertions(linkedClass)

  type Pat = PartialFunction[Tree, Boolean]

  abstract class AbstractIRNodeAssertions {
    protected def startTraverse(traverser: Traverser): Unit

    private def countTrees(pf: Pat): Int = {
      var count = 0
      val traverser = new Traverser {
        override def traverse(tree: Tree): Unit = {
          if (pf.applyOrElse(tree, (_: Tree) => false))
            count += 1
          super.traverse(tree)
        }
      }

      startTraverse(traverser)
      count
    }

    def has(trgName: String)(pf: Pat): this.type = {
      assertTrue(s"AST should have $trgName", countTrees(pf) > 0)
      this
    }

    def hasNot(trgName: String)(pf: Pat): this.type = {
      assertTrue(s"AST should not have $trgName", countTrees(pf) == 0)
      this
    }

    def hasExactly(count: Int, trgName: String)(pf: Pat): this.type = {
      val actualCount = countTrees(pf)
      assertEquals(s"AST has the wrong number of $trgName", count, actualCount)
      this
    }
  }

  class ClassDefAssertions(classDef: ClassDef) extends AbstractIRNodeAssertions {
    protected def startTraverse(traverser: Traverser): Unit =
      traverser.traverseClassDef(classDef)
  }

  class LinkedClassAssertions(linkedClass: LinkedClass) extends AbstractIRNodeAssertions {
    protected def startTraverse(traverser: Traverser): Unit = {
      linkedClass.jsSuperClass.foreach(traverser.traverse(_))
      linkedClass.fields.foreach(traverser.traverseMemberDef(_))
      linkedClass.methods.foreach(traverser.traverseMemberDef(_))
      linkedClass.jsConstructorDef.foreach(traverser.traverseMemberDef(_))
      linkedClass.exportedMembers.foreach(traverser.traverseMemberDef(_))
    }
  }
}
