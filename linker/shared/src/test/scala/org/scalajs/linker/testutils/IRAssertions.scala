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
import org.scalajs.ir.Definitions._
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

  type Pat = PartialFunction[IRNode, Boolean]

  private def patToTotal(pat: Pat): IRNode => Boolean =
    node => pat.applyOrElse(node, (_: IRNode) => false)

  abstract class AbstractIRNodeAssertions[T](selfNode: T) {
    protected def newTraverser(f: IRNode => Unit): TestTraverser[T]

    private def find(pf: Pat): Boolean =
      new TestFinder[T](patToTotal(pf))(newTraverser(_)).find(selfNode)

    def has(trgName: String)(pf: Pat): this.type = {
      assertTrue(s"AST should have $trgName", find(pf))
      this
    }

    def hasNot(trgName: String)(pf: Pat): this.type = {
      assertFalse(s"AST should not have $trgName", find(pf))
      this
    }

    def hasExactly(count: Int, trgName: String)(pf: Pat): this.type = {
      var actualCount = 0
      val traverser = newTraverser(patToTotal(pf).andThen { matches =>
        if (matches)
          actualCount += 1
      })
      traverser.traverse(selfNode)
      assertEquals(s"AST has the wrong number of $trgName", count, actualCount)
      this
    }
  }

  class ClassDefAssertions(classDef: ClassDef)
      extends AbstractIRNodeAssertions(classDef) {

    protected def newTraverser(f: IRNode => Unit): TestTraverser[ClassDef] = {
      new TestTraverser[ClassDef](f) {
        def baseTraverse(node: ClassDef): Unit = traverseClassDef(node)
      }
    }
  }

  class LinkedClassAssertions(linkedClass: LinkedClass)
      extends AbstractIRNodeAssertions(linkedClass) {

    protected def newTraverser(f: IRNode => Unit): TestTraverser[LinkedClass] = {
      new TestTraverser[LinkedClass](f) {
        def baseTraverse(node: LinkedClass): Unit = {
          for (memberDef <- node.fields)
            traverseMemberDef(memberDef)
          for (memberDef <- node.methods)
            traverseMemberDef(memberDef.value)
          for (memberDef <- node.exportedMembers)
            traverseMemberDef(memberDef.value)
          for (topLevelExportDef <- node.topLevelExports)
            traverseTopLevelExportDef(topLevelExportDef.value)
        }
      }
    }
  }

  abstract class TestTraverser[T](f: IRNode => Unit) extends Traverser {
    protected def baseTraverse(node: T): Unit

    def traverse(node: T): Unit =
      baseTraverse(node)

    override def traverse(tree: Tree): Unit = {
      f(tree)
      super.traverse(tree)
    }

    override def traverseClassDef(classDef: ClassDef): Unit = {
      f(classDef)
      super.traverseClassDef(classDef)
    }

    override def traverseMemberDef(memberDef: MemberDef): Unit = {
      f(memberDef)
      super.traverseMemberDef(memberDef)
    }

    override def traverseTopLevelExportDef(
        exportDef: TopLevelExportDef): Unit = {
      f(exportDef)
      super.traverseTopLevelExportDef(exportDef)
    }
  }

  final class TestFinder[T](f: IRNode => Boolean)(
      newTraverser: (IRNode => Unit) => TestTraverser[T]) {

    private case object Found extends ControlThrowable

    def find(node: T): Boolean = {
      try {
        newTraverser { innerNode =>
          if (f(innerNode))
            throw Found
        }.traverse(node)
        false
      } catch {
        case Found => true
      }
    }
  }
}
