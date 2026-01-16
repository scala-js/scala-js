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

package org.scalajs.testsuite.jsinterop

import scala.scalajs.js
import scala.scalajs.js.annotation._

import org.junit.Assert._
import org.junit.Test

class NewTargetTest {
  @Test def direct(): Unit = {
    class Direct extends js.Object {
      val inVal = js.`new`.target
      var inVar = js.`new`.target
      var inStat: js.Dynamic = _
      inStat = js.`new`.target
    }

    val direct = new Direct()
    assertSame(js.constructorOf[Direct], direct.inVal)
    assertSame(js.constructorOf[Direct], direct.inVar)
    assertSame(js.constructorOf[Direct], direct.inStat)
  }

  @Test def parent(): Unit = {
    class Parent extends js.Object {
      val inVal = js.`new`.target
      var inVar = js.`new`.target
      var inStat: js.Dynamic = _
      inStat = js.`new`.target
    }

    class Child extends Parent

    val child = new Child()
    assertSame(js.constructorOf[Child], child.inVal)
    assertSame(js.constructorOf[Child], child.inVar)
    assertSame(js.constructorOf[Child], child.inStat)
  }

  @Test def usableBeforeSuperConstructor(): Unit = {
    class BeforeSuperCtorBase(val newTarget: js.Dynamic) extends js.Object

    class BeforeSuperCtorParent extends BeforeSuperCtorBase(js.`new`.target)

    class BeforeSuperCtorChild extends BeforeSuperCtorParent

    val parent = new BeforeSuperCtorParent()
    assertSame(js.constructorOf[BeforeSuperCtorParent], parent.newTarget)

    val child = new BeforeSuperCtorChild()
    assertSame(js.constructorOf[BeforeSuperCtorChild], child.newTarget)
  }

  @Test def usableInSecondaryConstructor(): Unit = {
    class SecondaryCtorParent(val one: js.Dynamic) extends js.Object {
      var two: js.Dynamic = _

      def this() = {
        this(js.`new`.target)
        two = js.`new`.target
      }
    }

    class SecondaryCtorChild extends SecondaryCtorParent

    val parent = new SecondaryCtorParent()
    assertSame(js.constructorOf[SecondaryCtorParent], parent.one)
    assertSame(js.constructorOf[SecondaryCtorParent], parent.two)

    val child = new SecondaryCtorChild()
    assertSame(js.constructorOf[SecondaryCtorChild], child.one)
    assertSame(js.constructorOf[SecondaryCtorChild], child.two)
  }

  @Test def usableInObject(): Unit = {
    object ObjectWithNewTarget extends js.Object {
      val newTarget = js.`new`.target
    }

    assertSame(
        ObjectWithNewTarget.asInstanceOf[js.Dynamic].constructor, ObjectWithNewTarget.newTarget)
  }
}
