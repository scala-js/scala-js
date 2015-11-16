/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.scalalib

import scala.language.implicitConversions

import scala.reflect._

import scala.scalajs.js
import org.scalajs.jasminetest.JasmineTest

object ClassTagTest extends JasmineTest {

  implicit def class2jsAny(clazz: Class[_]): js.Any =
    clazz.asInstanceOf[js.Any]

  implicit def classTag2jsAny(ct: ClassTag[_]): js.Any =
    ct.asInstanceOf[js.Any]

  describe("scala.reflect.ClassTag") {

    it("apply() should get the existing instances for pre-defined ClassTags") {
      expect(ClassTag(classOf[Byte])).toBe(ClassTag.Byte)
      expect(ClassTag(classOf[Short])).toBe(ClassTag.Short)
      expect(ClassTag(classOf[Char])).toBe(ClassTag.Char)
      expect(ClassTag(classOf[Int])).toBe(ClassTag.Int)
      expect(ClassTag(classOf[Long])).toBe(ClassTag.Long)
      expect(ClassTag(classOf[Float])).toBe(ClassTag.Float)
      expect(ClassTag(classOf[Double])).toBe(ClassTag.Double)
      expect(ClassTag(classOf[Boolean])).toBe(ClassTag.Boolean)
      expect(ClassTag(classOf[Unit])).toBe(ClassTag.Unit)
      expect(ClassTag(classOf[Object])).toBe(ClassTag.Object)
      expect(ClassTag(classOf[Nothing])).toBe(ClassTag.Nothing)
      expect(ClassTag(classOf[Null])).toBe(ClassTag.Null)

      expect(classTag[Byte]).toBe(ClassTag.Byte)
      expect(classTag[Short]).toBe(ClassTag.Short)
      expect(classTag[Char]).toBe(ClassTag.Char)
      expect(classTag[Int]).toBe(ClassTag.Int)
      expect(classTag[Long]).toBe(ClassTag.Long)
      expect(classTag[Float]).toBe(ClassTag.Float)
      expect(classTag[Double]).toBe(ClassTag.Double)
      expect(classTag[Boolean]).toBe(ClassTag.Boolean)
      expect(classTag[Unit]).toBe(ClassTag.Unit)
      expect(classTag[Any]).toBe(ClassTag.Any)
      expect(classTag[Object]).toBe(ClassTag.Object)
      expect(classTag[AnyVal]).toBe(ClassTag.AnyVal)
      expect(classTag[AnyRef]).toBe(ClassTag.AnyRef)
      expect(classTag[Nothing]).toBe(ClassTag.Nothing)
      expect(classTag[Null]).toBe(ClassTag.Null)
    }

    it("runtimeClass") {
      expect(ClassTag.Byte.runtimeClass).toBe(classOf[Byte])
      expect(ClassTag.Short.runtimeClass).toBe(classOf[Short])
      expect(ClassTag.Char.runtimeClass).toBe(classOf[Char])
      expect(ClassTag.Int.runtimeClass).toBe(classOf[Int])
      expect(ClassTag.Long.runtimeClass).toBe(classOf[Long])
      expect(ClassTag.Float.runtimeClass).toBe(classOf[Float])
      expect(ClassTag.Double.runtimeClass).toBe(classOf[Double])
      expect(ClassTag.Boolean.runtimeClass).toBe(classOf[Boolean])
      expect(ClassTag.Unit.runtimeClass).toBe(classOf[Unit])
      expect(ClassTag.Any.runtimeClass).toBe(classOf[Any])
      expect(ClassTag.Object.runtimeClass).toBe(classOf[Object])
      expect(ClassTag.AnyVal.runtimeClass).toBe(classOf[AnyVal])
      expect(ClassTag.AnyRef.runtimeClass).toBe(classOf[AnyRef])
      expect(ClassTag.Nothing.runtimeClass).toBe(classOf[Nothing])
      expect(ClassTag.Null.runtimeClass).toBe(classOf[Null])

      expect(classTag[String].runtimeClass).toBe(classOf[String])
      expect(classTag[Integer].runtimeClass).toBe(classOf[Integer])
      expect(classTag[Seq[_]].runtimeClass).toBe(classOf[Seq[_]])

      expect(classTag[Array[_]].runtimeClass).toBe(classOf[Array[_]])
      expect(classTag[Array[Object]].runtimeClass).toBe(classOf[Array[Object]])
      expect(classTag[Array[_ <: AnyRef]].runtimeClass).toBe(classOf[Array[_ <: AnyRef]])
      expect(classTag[Array[String]].runtimeClass).toBe(classOf[Array[String]])
      expect(classTag[Array[_ <: Seq[_]]].runtimeClass).toBe(classOf[Array[_ <: Seq[_]]])
      expect(classTag[Array[Int]].runtimeClass).toBe(classOf[Array[Int]])
      expect(classTag[Array[Unit]].runtimeClass).toBe(classOf[Array[Unit]])

      // Weird, those two return Array[s.r.Nothing$] instead of Array[Object]
      // The same happens on the JVM
      expect(classTag[Array[Nothing]].runtimeClass).toBe(classOf[Array[scala.runtime.Nothing$]])
      expect(classTag[Array[Null]].runtimeClass).toBe(classOf[Array[scala.runtime.Null$]])

      expect(ClassTag(classOf[String]).runtimeClass).toBe(classOf[String])
      expect(ClassTag(classOf[Integer]).runtimeClass).toBe(classOf[Integer])
      expect(ClassTag(classOf[Seq[_]]).runtimeClass).toBe(classOf[Seq[_]])

      expect(ClassTag(classOf[Array[_]]).runtimeClass).toBe(classOf[Array[_]])
      expect(ClassTag(classOf[Array[Object]]).runtimeClass).toBe(classOf[Array[Object]])
      expect(ClassTag(classOf[Array[_ <: AnyRef]]).runtimeClass).toBe(classOf[Array[_ <: AnyRef]])
      expect(ClassTag(classOf[Array[String]]).runtimeClass).toBe(classOf[Array[String]])
      expect(ClassTag(classOf[Array[_ <: Seq[_]]]).runtimeClass).toBe(classOf[Array[_ <: Seq[_]]])
      expect(ClassTag(classOf[Array[Int]]).runtimeClass).toBe(classOf[Array[Int]])
      expect(ClassTag(classOf[Array[Unit]]).runtimeClass).toBe(classOf[Array[Unit]])

      // These work as expected, though
      expect(ClassTag(classOf[Array[Nothing]]).runtimeClass).toBe(classOf[Array[Nothing]])
      expect(ClassTag(classOf[Array[Null]]).runtimeClass).toBe(classOf[Array[Null]])
    }

  }

}
