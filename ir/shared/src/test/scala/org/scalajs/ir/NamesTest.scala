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

package org.scalajs.ir

import org.junit.Test
import org.junit.Assert._

import Names._
import Types._
import WellKnownNames._

class NamesTest {
  @Test def nameStringLocalName(): Unit = {
    assertEquals("foo", LocalName("foo").nameString)
    assertEquals(".this", LocalName.This.nameString)
  }

  @Test def nameStringLabelName(): Unit = {
    assertEquals("foo", LabelName("foo").nameString)
  }

  @Test def nameStringSimpleFieldName(): Unit = {
    assertEquals("foo", SimpleFieldName("foo").nameString)
  }

  @Test def nameStringFieldName(): Unit = {
    assertEquals("a.B::foo",
        FieldName(ClassName("a.B"), SimpleFieldName("foo")).nameString)
  }

  @Test def nameStringSimpleMethodName(): Unit = {
    assertEquals("foo", SimpleMethodName("foo").nameString)
    assertEquals("<init>", SimpleMethodName.Constructor.nameString)
    assertEquals("<stinit>", SimpleMethodName.StaticInitializer.nameString)
    assertEquals("<clinit>", SimpleMethodName.ClassInitializer.nameString)
  }

  @Test def nameStringMethodName(): Unit = {
    assertEquals("foo;I", MethodName("foo", Nil, IntRef).nameString)
    assertEquals("foo;Z;I", MethodName("foo", List(BooleanRef), IntRef).nameString)
    assertEquals("foo;Z;V", MethodName("foo", List(BooleanRef), VoidRef).nameString)

    assertEquals("foo;S;Ljava.io.Serializable;V",
        MethodName("foo", List(ShortRef, ClassRef(SerializableClass)), VoidRef).nameString)

    assertEquals("<init>;I;V", MethodName.constructor(List(IntRef)).nameString)

    assertEquals("foo;Z;R", MethodName.reflectiveProxy("foo", List(BooleanRef)).nameString)

    val refAndNameStrings: List[(TypeRef, String)] = List(
      ClassRef(ObjectClass) -> "Ljava.lang.Object",
      ClassRef(SerializableClass) -> "Ljava.io.Serializable",
      ClassRef(BoxedStringClass) -> "Ljava.lang.String",
      ArrayTypeRef(ClassRef(ObjectClass), 2) -> "[[Ljava.lang.Object",
      ArrayTypeRef(ShortRef, 1) -> "[S"
    )
    for ((ref, nameString) <- refAndNameStrings) {
      assertEquals(s"foo;$nameString;V",
          MethodName("foo", List(ref), VoidRef).nameString)
    }
  }

  @Test def nameStringClassName(): Unit = {
    assertEquals("a.B", ClassName("a.B").nameString)
  }
}
