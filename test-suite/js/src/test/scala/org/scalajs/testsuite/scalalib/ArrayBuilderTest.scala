/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013--2015, LAMP/EPFL  **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.scalalib

import scala.language.implicitConversions

import scala.reflect._
import scala.collection.mutable.ArrayBuilder

import scala.scalajs.js
import org.scalajs.jasminetest.JasmineTest

object ArrayBuilderTest extends JasmineTest {

  implicit def class2jsAny(clazz: Class[_]): js.Any =
    clazz.asInstanceOf[js.Any]

  implicit def classTag2jsAny(ct: ClassTag[_]): js.Any =
    ct.asInstanceOf[js.Any]

  @noinline
  def erase(x: Any): Any = x

  @inline
  def makeNoInline[T: ClassTag](): ArrayBuilder[T] = {
    @noinline def ct = implicitly[ClassTag[T]]
    ArrayBuilder.make[T]()(ct)
  }

  @inline
  def zerosInline[T: ClassTag](length: Int): Array[T] =
    Array.fill(length)(null.asInstanceOf[T])

  @noinline
  def zerosNoInline[T: ClassTag](length: Int): Array[T] =
    Array.fill(length)(null.asInstanceOf[T])

  @noinline def someInt: Int = 53
  @noinline def someChar: Char = 'S'
  @noinline def someBoolean: Boolean = false
  @noinline def someString: String = "world"

  describe("scala.collection.mutable.ArrayBuilder") {

    it("Int, normal case inline") {
      val b = ArrayBuilder.make[Int]()
      b += 42
      b += someInt
      val a = b.result()

      expect(a.getClass).toBe(classOf[Array[Int]])
      expect(a.length).toEqual(2)
      expect(erase(a(0)).isInstanceOf[Int]).toBeTruthy
      expect(erase(a(0)).equals(42)).toBeTruthy
      expect(erase(a(1)).equals(53)).toBeTruthy
    }

    it("Int, normal case noinline") {
      val b = makeNoInline[Int]()
      b += 42
      b += someInt
      val a = b.result()

      expect(a.getClass).toBe(classOf[Array[Int]])
      expect(a.length).toEqual(2)
      expect(erase(a(0)).isInstanceOf[Int]).toBeTruthy
      expect(erase(a(0)).equals(42)).toBeTruthy
      expect(erase(a(1)).equals(53)).toBeTruthy
    }

    it("Int, zeros inline") {
      val a = zerosInline[Int](3)
      expect(a.getClass).toBe(classOf[Array[Int]])
      expect(a.length).toEqual(3)
      expect(erase(a(0)).isInstanceOf[Int]).toBeTruthy
      expect(erase(a(0)) == 0).toBeTruthy
    }

    it("Int, zeros noinline") {
      val a = zerosNoInline[Int](3)
      expect(a.getClass).toBe(classOf[Array[Int]])
      expect(a.length).toEqual(3)
      expect(erase(a(0)).isInstanceOf[Int]).toBeTruthy
      expect(erase(a(0)) == 0).toBeTruthy
    }

    it("Char, normal case inline") {
      val b = ArrayBuilder.make[Char]()
      b += 'A'
      b += someChar
      val a = b.result()

      expect(a.getClass).toBe(classOf[Array[Char]])
      expect(a.length).toEqual(2)
      expect(erase(a(0)).isInstanceOf[Char]).toBeTruthy
      expect(erase(a(0)).equals('A')).toBeTruthy
      expect(erase(a(1)).equals('S')).toBeTruthy
    }

    it("Char, normal case noinline") {
      val b = makeNoInline[Char]()
      b += 'A'
      b += someChar
      val a = b.result()

      expect(a.getClass).toBe(classOf[Array[Char]])
      expect(a.length).toEqual(2)
      expect(erase(a(0)).isInstanceOf[Char]).toBeTruthy
      expect(erase(a(0)).equals('A')).toBeTruthy
      expect(erase(a(1)).equals('S')).toBeTruthy
    }

    it("Char, zeros inline") {
      val a = zerosInline[Char](3)
      expect(a.getClass).toBe(classOf[Array[Char]])
      expect(a.length).toEqual(3)
      expect(erase(a(0)).isInstanceOf[Char]).toBeTruthy
      expect(erase(a(0)).equals('\0')).toBeTruthy
    }

    it("Char, zeros noinline") {
      val a = zerosNoInline[Char](3)
      expect(a.getClass).toBe(classOf[Array[Char]])
      expect(a.length).toEqual(3)
      expect(erase(a(0)).isInstanceOf[Char]).toBeTruthy
      expect(erase(a(0)).equals('\0')).toBeTruthy
    }

    it("Boolean, normal case inline") {
      val b = ArrayBuilder.make[Boolean]()
      b += true
      b += someBoolean
      val a = b.result()

      expect(a.getClass).toBe(classOf[Array[Boolean]])
      expect(a.length).toEqual(2)
      expect(erase(a(0)).isInstanceOf[Boolean]).toBeTruthy
      expect(erase(a(0)).equals(true)).toBeTruthy
      expect(erase(a(1)).equals(false)).toBeTruthy
    }

    it("Boolean, normal case noinline") {
      val b = makeNoInline[Boolean]()
      b += true
      b += someBoolean
      val a = b.result()

      expect(a.getClass).toBe(classOf[Array[Boolean]])
      expect(a.length).toEqual(2)
      expect(erase(a(0)).isInstanceOf[Boolean]).toBeTruthy
      expect(erase(a(0)).equals(true)).toBeTruthy
      expect(erase(a(1)).equals(false)).toBeTruthy
    }

    it("Boolean, zeros inline") {
      val a = zerosInline[Boolean](3)
      expect(a.getClass).toBe(classOf[Array[Boolean]])
      expect(a.length).toEqual(3)
      expect(erase(a(0)).isInstanceOf[Boolean]).toBeTruthy
      expect(erase(a(0)) == false).toBeTruthy // scalastyle:ignore
    }

    it("Boolean, zeros noinline") {
      val a = zerosNoInline[Boolean](3)
      expect(a.getClass).toBe(classOf[Array[Boolean]])
      expect(a.length).toEqual(3)
      expect(erase(a(0)).isInstanceOf[Boolean]).toBeTruthy
      expect(erase(a(0)) == false).toBeTruthy // scalastyle:ignore
    }

    it("Unit, normal case inline") {
      val b = ArrayBuilder.make[Unit]()
      b += (())
      val a = b.result()

      expect(a.getClass).toBe(classOf[Array[Unit]])
      expect(a.length).toEqual(1)
      expect(erase(a(0)).isInstanceOf[Unit]).toBeTruthy
      expect(erase(a(0)).equals(())).toBeTruthy
    }

    it("Unit, normal case noinline") {
      val b = makeNoInline[Unit]()
      b += (())
      val a = b.result()

      expect(a.getClass).toBe(classOf[Array[Unit]])
      expect(a.length).toEqual(1)
      expect(erase(a(0)).isInstanceOf[Unit]).toBeTruthy
      expect(erase(a(0)).equals(())).toBeTruthy
    }

    it("Unit, zeros inline") {
      val a = zerosInline[Unit](3)
      expect(a.getClass).toBe(classOf[Array[Unit]])
      expect(a.length).toEqual(3)
      expect(erase(a(0)).isInstanceOf[Unit]).toBeTruthy // JVM says false
      expect(erase(a(0)) == (())).toBeTruthy            // JVM says false
    }

    it("Unit, zeros noinline") {
      val a = zerosNoInline[Unit](3)
      expect(a.getClass).toBe(classOf[Array[Unit]])
      expect(a.length).toEqual(3)
      expect(erase(a(0)).isInstanceOf[Unit]).toBeTruthy // JVM says false
      expect(erase(a(0)) == (())).toBeTruthy            // JVM says false
    }

    it("String, normal case inline") {
      val b = ArrayBuilder.make[String]()
      b += "hello"
      b += someString
      val a = b.result()

      expect(a.getClass).toBe(classOf[Array[String]])
      expect(a.length).toEqual(2)
      expect(erase(a(0)).isInstanceOf[String]).toBeTruthy
      expect(erase(a(0)).equals("hello")).toBeTruthy
      expect(erase(a(1)).equals("world")).toBeTruthy
    }

    it("String, normal case noinline") {
      val b = makeNoInline[String]()
      b += "hello"
      b += someString
      val a = b.result()

      expect(a.getClass).toBe(classOf[Array[String]])
      expect(a.length).toEqual(2)
      expect(erase(a(0)).isInstanceOf[String]).toBeTruthy
      expect(erase(a(0)).equals("hello")).toBeTruthy
      expect(erase(a(1)).equals("world")).toBeTruthy
    }

    it("String, zeros inline") {
      val a = zerosInline[String](3)
      expect(a.getClass).toBe(classOf[Array[String]])
      expect(a.length).toEqual(3)
      expect(erase(a(0)).asInstanceOf[js.Any]).toBeNull
      expect(erase(a(0)) == null).toBeTruthy
    }

    it("String, zeros noinline") {
      val a = zerosNoInline[String](3)
      expect(a.getClass).toBe(classOf[Array[String]])
      expect(a.length).toEqual(3)
      expect(erase(a(0)).asInstanceOf[js.Any]).toBeNull
      expect(erase(a(0)) == null).toBeTruthy
    }

    it("Nothing and Null") {
      expect(ArrayBuilder.make[Nothing]().result().getClass).toBe(classOf[Array[Nothing]])
      expect(ArrayBuilder.make[Null]().result().getClass).toBe(classOf[Array[Null]])

      expect(makeNoInline[Nothing]().result().getClass).toBe(classOf[Array[Nothing]])
      expect(makeNoInline[Null]().result().getClass).toBe(classOf[Array[Null]])
    }

  }

}
