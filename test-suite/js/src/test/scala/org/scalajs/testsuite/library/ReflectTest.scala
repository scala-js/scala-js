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

package org.scalajs.testsuite.library

import org.junit.Assert._
import org.junit.Assume._
import org.junit.Test

import org.scalajs.testsuite.utils.AssertThrows.assertThrows
import org.scalajs.testsuite.utils.Platform._

import scala.scalajs.reflect._
import scala.scalajs.reflect.annotation._

class ReflectTest {
  import ReflectTest.{Accessors, VC}

  private final val Prefix = "org.scalajs.testsuite.library.ReflectTest$"

  private final val NameClassEnableDirect =
    Prefix + "ClassEnableDirect"
  private final val NameClassEnableDirectNoZeroArgCtor =
    Prefix + "ClassEnableDirectNoZeroArgCtor"
  private final val NameObjectEnableDirect =
    Prefix + "ObjectEnableDirect$"
  private final val NameTraitEnableDirect =
    Prefix + "TraitEnableDirect"
  private final val NameAbstractClassEnableDirect =
    Prefix + "AbstractClassEnableDirect"
  private final val NameClassNoPublicConstructorEnableDirect =
    Prefix + "ClassNoPublicConstructorEnableDirect"

  private final val NameInnerClass = {
    Prefix + "ClassWithInnerClassWithEnableReflectiveInstantiation$" +
    "InnerClassWithEnableReflectiveInstantiation"
  }

  private final val NameClassEnableIndirect =
    Prefix + "ClassEnableIndirect"
  private final val NameClassEnableIndirectNoZeroArgCtor =
    Prefix + "ClassEnableIndirectNoZeroArgCtor"
  private final val NameObjectEnableIndirect =
    Prefix + "ObjectEnableIndirect$"
  private final val NameTraitEnableIndirect =
    Prefix + "TraitEnableIndirect"
  private final val NameAbstractClassEnableIndirect =
    Prefix + "AbstractClassEnableIndirect"
  private final val NameClassNoPublicConstructorEnableIndirect =
    Prefix + "ClassNoPublicConstructorEnableIndirect"

  private final val NameClassDisable =
    Prefix + "ClassDisable"
  private final val NameObjectDisable =
    Prefix + "ObjectDisable$"
  private final val NameTraitDisable =
    Prefix + "TraitDisable"

  private final val NameInnerObject = {
    Prefix + "ClassWithInnerObjectWithEnableReflectiveInstantiation$" +
    "InnerObjectWithEnableReflectiveInstantiation"
  }

  @Test def testClassRuntimeClass(): Unit = {
    for {
      name <- Seq(NameClassEnableDirect, NameClassEnableDirectNoZeroArgCtor,
          NameClassEnableIndirect, NameClassEnableIndirectNoZeroArgCtor)
    } {
      val optClassData = Reflect.lookupInstantiatableClass(name)
      assertTrue(optClassData.isDefined)
      val classData = optClassData.get

      val runtimeClass = optClassData.get.runtimeClass
      assertEquals(name, runtimeClass.getName)
    }
  }

  @Test def testObjectRuntimeClass(): Unit = {
    for {
      name <- Seq(NameObjectEnableDirect, NameObjectEnableIndirect)
    } {
      val optClassData = Reflect.lookupLoadableModuleClass(name)
      assertTrue(optClassData.isDefined)
      val classData = optClassData.get

      val runtimeClass = optClassData.get.runtimeClass
      assertEquals(name, runtimeClass.getName)
    }
  }

  @Test def testClassCannotBeFound(): Unit = {
    for {
      name <- Seq(NameObjectEnableDirect, NameTraitEnableDirect,
          NameAbstractClassEnableDirect,
          NameClassNoPublicConstructorEnableDirect, NameObjectEnableIndirect,
          NameTraitEnableIndirect, NameAbstractClassEnableIndirect,
          NameClassNoPublicConstructorEnableIndirect, NameClassDisable,
          NameObjectDisable, NameTraitDisable)
    } {
      assertFalse(s"$name should not be found",
          Reflect.lookupInstantiatableClass(name).isDefined)
    }
  }

  @Test def testObjectCannotBeFound(): Unit = {
    for {
      name <- Seq(NameClassEnableDirect, NameClassEnableDirectNoZeroArgCtor,
          NameTraitEnableDirect, NameAbstractClassEnableDirect,
          NameClassNoPublicConstructorEnableDirect, NameClassEnableIndirect,
          NameTraitEnableIndirect, NameAbstractClassEnableIndirect,
          NameClassNoPublicConstructorEnableIndirect, NameClassDisable,
          NameObjectDisable, NameTraitDisable)
    } {
      assertFalse(s"$name should not be found",
          Reflect.lookupLoadableModuleClass(name).isDefined)
    }
  }

  @Test def testClassNoArgCtor(): Unit = {
    for (name <- Seq(NameClassEnableDirect, NameClassEnableIndirect)) {
      val optClassData = Reflect.lookupInstantiatableClass(name)
      assertTrue(optClassData.isDefined)
      val classData = optClassData.get

      val instance = classData.newInstance().asInstanceOf[Accessors]
      assertEquals(-1, instance.x)
      assertEquals(name.stripPrefix(Prefix), instance.y)
    }
  }

  @Test def testClassNoArgCtorErrorCase(): Unit = {
    for (name <- Seq(NameClassEnableDirectNoZeroArgCtor,
            NameClassEnableIndirectNoZeroArgCtor)) {
      val optClassData = Reflect.lookupInstantiatableClass(name)
      assertTrue(optClassData.isDefined)
      val classData = optClassData.get

      assertThrows(classOf[InstantiationException], {
        classData.newInstance()
      })
    }
  }

  @Test def testClassCtorWithArgs(): Unit = {
    for (name <- Seq(NameClassEnableDirect, NameClassEnableDirectNoZeroArgCtor,
            NameClassEnableIndirect, NameClassEnableIndirectNoZeroArgCtor)) {
      val optClassData = Reflect.lookupInstantiatableClass(name)
      assertTrue(optClassData.isDefined)
      val classData = optClassData.get

      val optCtorIntString =
        classData.getConstructor(classOf[Int], classOf[String])
      assertTrue(optCtorIntString.isDefined)
      val instanceIntString =
        optCtorIntString.get.newInstance(543, "foobar").asInstanceOf[Accessors]
      assertEquals(543, instanceIntString.x)
      assertEquals("foobar", instanceIntString.y)

      val optCtorInt = classData.getConstructor(classOf[Int])
      assertTrue(optCtorInt.isDefined)
      val instanceInt =
        optCtorInt.get.newInstance(123).asInstanceOf[Accessors]
      assertEquals(123, instanceInt.x)
      assertEquals(name.stripPrefix(Prefix), instanceInt.y)

      // Value class is seen as its underlying
      val optCtorShort = classData.getConstructor(classOf[Short])
      assertTrue(optCtorShort.isDefined)
      val instanceShort =
        optCtorShort.get.newInstance(21).asInstanceOf[Accessors]
      assertEquals(42, instanceShort.x)
      assertEquals(name.stripPrefix(Prefix), instanceShort.y)

      // Non-existent
      assertFalse(classData.getConstructor(classOf[Boolean]).isDefined)
      assertFalse(classData.getConstructor(classOf[VC]).isDefined)

      // Non-public
      assertFalse(classData.getConstructor(classOf[String]).isDefined)
      assertFalse(classData.getConstructor(classOf[Double]).isDefined)
    }
  }

  @Test def testInnerClass(): Unit = {
    import ReflectTest.ClassWithInnerClassWithEnableReflectiveInstantiation

    val outer = new ClassWithInnerClassWithEnableReflectiveInstantiation(15)

    val optClassData = Reflect.lookupInstantiatableClass(NameInnerClass)
    assertTrue(optClassData.isDefined)
    val classData = optClassData.get

    val optCtorOuterString =
      classData.getConstructor(outer.getClass, classOf[String])
    assertTrue(optCtorOuterString.isDefined)
    val instanceOuterString =
      optCtorOuterString.get.newInstance(outer, "babar").asInstanceOf[Accessors]
    assertEquals(15, instanceOuterString.x)
    assertEquals("babar", instanceOuterString.y)
  }

  @Test def testLocalClass(): Unit = {
    @EnableReflectiveInstantiation
    class LocalClassWithEnableReflectiveInstantiation

    val fqcn = classOf[LocalClassWithEnableReflectiveInstantiation].getName
    assertFalse(s"$fqcn should not be found",
        Reflect.lookupInstantiatableClass(fqcn).isDefined)
  }

  @Test def testObjectLoad(): Unit = {
    for (name <- Seq(NameObjectEnableDirect, NameObjectEnableIndirect)) {
      val optClassData = Reflect.lookupLoadableModuleClass(name)
      assertTrue(optClassData.isDefined)
      val classData = optClassData.get

      val instance = classData.loadModule().asInstanceOf[Accessors]
      assertEquals(101, instance.x)
      assertEquals(name.stripPrefix(Prefix), instance.y)
    }
  }

  @Test def testInnerObjectWithEnableReflectiveInstantiation_Issue3228(): Unit = {
    assertFalse(Reflect.lookupLoadableModuleClass(NameInnerObject).isDefined)
    assertFalse(Reflect.lookupInstantiatableClass(NameInnerObject).isDefined)
  }

  @Test def testLocalClassWithReflectiveInstantiationInLambda_Issue3227(): Unit = {
    // Test that the presence of the following code does not prevent linking
    { () =>
      @EnableReflectiveInstantiation
      class Foo
    }
  }

}

object ReflectTest {
  trait Accessors {
    val x: Int
    val y: String
  }

  final class VC(val self: Short) extends AnyVal

  // Entities with directly enabled reflection

  @EnableReflectiveInstantiation
  class ClassEnableDirect(val x: Int, val y: String) extends Accessors {
    def this(x: Int) = this(x, "ClassEnableDirect")
    def this() = this(-1)
    def this(vc: VC) = this(vc.self.toInt * 2)

    protected def this(y: String) = this(-5, y)
    private def this(d: Double) = this(d.toInt)
  }

  @EnableReflectiveInstantiation
  class ClassEnableDirectNoZeroArgCtor(val x: Int, val y: String)
      extends Accessors {
    def this(x: Int) = this(x, "ClassEnableDirectNoZeroArgCtor")
    def this(vc: VC) = this(vc.self.toInt * 2)

    protected def this(y: String) = this(-5, y)
    private def this(d: Double) = this(d.toInt)
  }

  @EnableReflectiveInstantiation
  object ObjectEnableDirect extends Accessors {
    val x = 101
    val y = "ObjectEnableDirect$"
  }

  @EnableReflectiveInstantiation
  trait TraitEnableDirect extends Accessors

  @EnableReflectiveInstantiation
  abstract class AbstractClassEnableDirect(val x: Int, val y: String)
      extends Accessors {

    def this(x: Int) = this(x, "AbstractClassEnableDirect")
    def this() = this(-1)
    def this(vc: VC) = this(vc.self.toInt * 2)

    protected def this(y: String) = this(-5, y)
    private def this(d: Double) = this(d.toInt)
  }

  @EnableReflectiveInstantiation
  class ClassNoPublicConstructorEnableDirect private (val x: Int, val y: String)
      extends Accessors {

    protected def this(y: String) = this(-5, y)
  }

  class ClassWithInnerClassWithEnableReflectiveInstantiation(_x: Int) {
    @EnableReflectiveInstantiation
    class InnerClassWithEnableReflectiveInstantiation(_y: String)
        extends Accessors {
      val x = _x
      val y = _y
    }
  }

  // Entities with reflection enabled by inheritance

  @EnableReflectiveInstantiation
  trait EnablingTrait

  class ClassEnableIndirect(val x: Int, val y: String)
      extends EnablingTrait with Accessors {

    def this(x: Int) = this(x, "ClassEnableIndirect")
    def this() = this(-1)
    def this(vc: VC) = this(vc.self.toInt * 2)

    protected def this(y: String) = this(-5, y)
    private def this(d: Double) = this(d.toInt)
  }

  class ClassEnableIndirectNoZeroArgCtor(val x: Int, val y: String)
      extends EnablingTrait with Accessors {
    def this(x: Int) = this(x, "ClassEnableIndirectNoZeroArgCtor")
    def this(vc: VC) = this(vc.self.toInt * 2)

    protected def this(y: String) = this(-5, y)
    private def this(d: Double) = this(d.toInt)
  }

  object ObjectEnableIndirect extends EnablingTrait with Accessors {
    val x = 101
    val y = "ObjectEnableIndirect$"
  }

  trait TraitEnableIndirect extends EnablingTrait with Accessors

  abstract class AbstractClassEnableIndirect(val x: Int, val y: String)
      extends EnablingTrait with Accessors {

    def this(x: Int) = this(x, "AbstractClassEnableIndirect")
    def this() = this(-1)
    def this(vc: VC) = this(vc.self.toInt * 2)

    protected def this(y: String) = this(-5, y)
    private def this(d: Double) = this(d.toInt)
  }

  class ClassNoPublicConstructorEnableIndirect private (
      val x: Int, val y: String)
      extends EnablingTrait with Accessors {

    protected def this(y: String) = this(-5, y)
  }

  // Entities with reflection disabled

  class ClassDisable(val x: Int, val y: String) extends Accessors

  object ObjectDisable extends Accessors {
    val x = 101
    val y = "ObjectDisable$"
  }

  trait TraitDisable extends Accessors

  // Regression cases

  class ClassWithInnerObjectWithEnableReflectiveInstantiation {
    @EnableReflectiveInstantiation
    object InnerObjectWithEnableReflectiveInstantiation
  }
}
