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

package org.scalajs.linker.frontend

import org.junit.Test
import org.junit.Assert._

import org.scalajs.ir.Names._
import org.scalajs.ir.Trees.NewLambda.Descriptor
import org.scalajs.ir.Types._
import org.scalajs.ir.WellKnownNames._

import org.scalajs.linker.testutils.TestIRBuilder._

class LambdaSynthesizerTest {
  private def makeDesc(superClass: ClassName, interfaces: List[ClassName],
      methodName: String, paramTypeRefs: List[TypeRef], resultTypeRef: TypeRef): Descriptor = {

    // Only for tests; would not work for JS class types
    def typeRefToType(typeRef: TypeRef): Type = typeRef match {
      case typeRef: PrimRef          => typeRef.tpe
      case ClassRef(className)       => ClassType(className, nullable = true, exact = false)
      case typeRef: ArrayTypeRef     => ArrayType(typeRef, nullable = true, exact = false)
      case typeRef: TransientTypeRef => typeRef.tpe
    }

    Descriptor(superClass, interfaces,
        MethodName(SimpleMethodName(methodName), paramTypeRefs, resultTypeRef),
        paramTypeRefs.map(typeRefToType(_)), typeRefToType(resultTypeRef))
  }

  private def makeClassName(superClass: ClassName, interfaces: List[ClassName],
      methodName: String, paramTypeRefs: List[TypeRef], resultTypeRef: TypeRef): String = {
    val desc = makeDesc(superClass, interfaces, methodName, paramTypeRefs, resultTypeRef)
    LambdaSynthesizer.makeClassName(desc).nameString
  }

  @Test def testMakeClassNameBasicShape(): Unit = {
    assertEquals(
        "java.lang.Comparable.$$Lambda$fe61a0e160a176edef6be9e0784c781844b58dc4",
        makeClassName(ObjectClass, List("java.lang.Comparable"), "compareTo", List(O), I))

    assertEquals(
        "scala.runtime.AbstractFunction1.$$Lambda$35cb327daad2f78934715043b1178369d7894759",
        makeClassName("scala.runtime.AbstractFunction1", Nil, "apply", List(O), O))
  }

  @Test def testMakeClassNameEveryBitMatters(): Unit = {
    val IClass = ClassRef("I")
    val CClass = ClassRef("C")

    val descs: Vector[Descriptor] = Vector(
      makeDesc(ObjectClass, List("I"), "foo", List(IClass), I),
      makeDesc("A", List("I"), "foo", List(IClass), I),
      makeDesc(ObjectClass, List("I"), "foo", List(IClass, CharRef), I),
      makeDesc(ObjectClass, List("J"), "foo", List(IClass), I),
      makeDesc(ObjectClass, List("I"), "bar", List(IClass), I),
      makeDesc(ObjectClass, List("I"), "foo", List(CClass), I),
      makeDesc(ObjectClass, List("I"), "foo", List(IClass), Z),
      makeDesc(ObjectClass, List("I"), "foo", List(IClass), IClass),
      makeDesc(ObjectClass, List("I"), "foo", List(CClass), IClass),
      makeDesc(ObjectClass, List("I"), "foo", List(IClass), V),
      makeDesc(ObjectClass, List("I"), "foo", List(IClass), ArrayTypeRef(I, 1)),
      makeDesc(ObjectClass, List("I"), "foo", List(IClass), ArrayTypeRef(IClass, 1)),
      makeDesc(ObjectClass, List("I"), "foo", List(IClass), ArrayTypeRef(I, 3)),
      makeDesc(ObjectClass, List("I"), "foo", List(IClass), ArrayTypeRef(IClass, 3)),
      makeDesc(ObjectClass, List("I"), "foo", List(IClass), TransientTypeRef(LabelName("I"))(IntType))
    )

    val classNames = descs.map(LambdaSynthesizer.makeClassName(_))

    for {
      i <- 0 until descs.size
      j <- i + 1 until descs.size
    } {
      if (classNames(i) == classNames(j)) {
        fail(
            "Two descriptors hashed to the same class name:\n" +
            s"${descs(i)}\n${descs(j)}\n${classNames(i).nameString}")
      }
    }
  }
}
