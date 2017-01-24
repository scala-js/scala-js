/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2017, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.library

import org.junit.Assert._
import org.junit.Assume._
import org.junit.Test

import org.scalajs.testsuite.utils.AssertThrows._
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
}
