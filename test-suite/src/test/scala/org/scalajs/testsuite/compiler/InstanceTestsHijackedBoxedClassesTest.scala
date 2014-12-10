/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.compiler

import org.scalajs.jasminetest.JasmineTest
import scala.scalajs.js

object InstanceTestsHijackedBoxedClassesTest extends JasmineTest {

  describe("Instance tests for hijacked boxed classes") {

    it("should support isInstanceOf (positive)") {
      expect((()         : Any).isInstanceOf[Unit]   ).toBeTruthy
      expect((false      : Any).isInstanceOf[Boolean]).toBeTruthy
      expect(('a'        : Any).isInstanceOf[Char]   ).toBeTruthy
      expect((65.toByte  : Any).isInstanceOf[Byte]   ).toBeTruthy
      expect((654.toShort: Any).isInstanceOf[Short]  ).toBeTruthy
      expect((-4321      : Any).isInstanceOf[Int]    ).toBeTruthy
      expect((684321L    : Any).isInstanceOf[Long]   ).toBeTruthy
      expect((3.14f      : Any).isInstanceOf[Float]  ).toBeTruthy
      expect((3.14       : Any).isInstanceOf[Double] ).toBeTruthy

      expect((45  : Any).isInstanceOf[Float] ).toBeTruthy
      expect((45  : Any).isInstanceOf[Double]).toBeTruthy
      expect((3.0f: Any).isInstanceOf[Int]   ).toBeTruthy
      expect((3.0f: Any).isInstanceOf[Double]).toBeTruthy
      expect((5.0 : Any).isInstanceOf[Int]   ).toBeTruthy
      expect((5.0 : Any).isInstanceOf[Float] ).toBeTruthy
    }

    it("should support isInstanceOf (negative)") {
      expect((12345: Any).isInstanceOf[Unit]   ).toBeFalsy
      expect((12345: Any).isInstanceOf[Boolean]).toBeFalsy
      expect((12345: Any).isInstanceOf[Char]   ).toBeFalsy
      expect(('a'  : Any).isInstanceOf[Byte]   ).toBeFalsy
      expect(('b'  : Any).isInstanceOf[Short]  ).toBeFalsy
      expect(('c'  : Any).isInstanceOf[Int]    ).toBeFalsy
      expect(('d'  : Any).isInstanceOf[Long]   ).toBeFalsy
      expect(('f'  : Any).isInstanceOf[Float]  ).toBeFalsy
      expect(('g'  : Any).isInstanceOf[Double] ).toBeFalsy
    }

    it("should support asInstanceOf (positive)") {
      def swallow(x: Any): Unit = ()
      swallow((()         : Any).asInstanceOf[Unit]   )
      swallow((false      : Any).asInstanceOf[Boolean])
      swallow(('a'        : Any).asInstanceOf[Char]   )
      swallow((65.toByte  : Any).asInstanceOf[Byte]   )
      swallow((654.toShort: Any).asInstanceOf[Short]  )
      swallow((-4321      : Any).asInstanceOf[Int]    )
      swallow((684321L    : Any).asInstanceOf[Long]   )
      swallow((3.14f      : Any).asInstanceOf[Float]  )
      swallow((3.14       : Any).asInstanceOf[Double] )
    }

    when("compliant-asinstanceof").
    it("should support asInstanceOf (negative)") {
      expect(() => (12345: Any).asInstanceOf[Unit]   ).toThrow
      expect(() => (12345: Any).asInstanceOf[Boolean]).toThrow
      expect(() => (12345: Any).asInstanceOf[Char]   ).toThrow
      expect(() => ('a'  : Any).asInstanceOf[Byte]   ).toThrow
      expect(() => ('b'  : Any).asInstanceOf[Short]  ).toThrow
      expect(() => ('c'  : Any).asInstanceOf[Int]    ).toThrow
      expect(() => ('d'  : Any).asInstanceOf[Long]   ).toThrow
      expect(() => ('f'  : Any).asInstanceOf[Float]  ).toThrow
      expect(() => ('g'  : Any).asInstanceOf[Double] ).toThrow
    }

    it("should support isInstanceOf via java.lang.Class (positive)") {
      def test(x: Any, clazz: Class[_]): Unit =
        expect(clazz.isInstance(x)).toBeTruthy

      test(()         , classOf[scala.runtime.BoxedUnit])
      test(false      , classOf[java.lang.Boolean])
      test('a'        , classOf[java.lang.Character])
      test(65.toByte  , classOf[java.lang.Byte])
      test(654.toShort, classOf[java.lang.Short])
      test(-4321      , classOf[java.lang.Integer])
      test(684321L    , classOf[java.lang.Long])
      test(3.14f      , classOf[java.lang.Float])
      test(3.14       , classOf[java.lang.Double])
    }

    it("should support isInstanceOf via java.lang.Class (negative)") {
      def test(x: Any, clazz: Class[_]): Unit =
        expect(clazz.isInstance(x)).toBeFalsy

      test(12345, classOf[scala.runtime.BoxedUnit])
      test(12345, classOf[java.lang.Boolean])
      test(12345, classOf[java.lang.Character])
      test('a'  , classOf[java.lang.Byte])
      test('b'  , classOf[java.lang.Short])
      test('c'  , classOf[java.lang.Integer])
      test('d'  , classOf[java.lang.Long])
      test('e'  , classOf[java.lang.Float])
      test('f'  , classOf[java.lang.Double])
    }

  }
}
