package org.scalajs.testsuite.javalib.util

import java.{util => ju}

import org.scalajs.jasminetest.JasmineTest
import org.scalajs.testsuite.javalib.ExpectExceptions


object ObjectsTestOnJDK7 extends JasmineTest with ExpectExceptions {

  describe("java.util.Objects") {

    it("should provide `equals`") {
      val obj = new Object
      expect(ju.Objects.equals(null, null)).toBeTruthy
      expect(ju.Objects.equals(null, obj)).toBeFalsy
      expect(ju.Objects.equals(obj, null)).toBeFalsy
      expect(ju.Objects.equals(obj, obj)).toBeTruthy
      expect(ju.Objects.equals(new Object, new Object)).toBeFalsy
      expect(ju.Objects.equals(1, 1)).toBeTruthy
      expect(ju.Objects.equals(1, 2)).toBeFalsy
      expect(ju.Objects.equals("abc", "abc")).toBeTruthy
      expect(ju.Objects.equals("abc", "abd")).toBeFalsy
    }

    it("should provide `deepEquals`") {
      val obj = new Object
      expect(ju.Objects.deepEquals(null, null)).toBeTruthy
      expect(ju.Objects.deepEquals(null, obj)).toBeFalsy
      expect(ju.Objects.deepEquals(obj, null)).toBeFalsy
      expect(ju.Objects.deepEquals(obj, obj)).toBeTruthy
      expect(ju.Objects.deepEquals(new Object, new Object)).toBeFalsy
      expect(ju.Objects.deepEquals(1, 1)).toBeTruthy
      expect(ju.Objects.deepEquals(1, 2)).toBeFalsy
      expect(ju.Objects.deepEquals("abc", "abc")).toBeTruthy
      expect(ju.Objects.deepEquals("abc", "abd")).toBeFalsy
      expect(ju.Objects.deepEquals(Array(Array(1)), Array(Array(1)))).toBeTruthy
    }

    it("should provide `hashCode`") {
      val obj = new Object
      expect(ju.Objects.hashCode(null)).toEqual(0)
      expect(ju.Objects.hashCode(obj)).toEqual(obj.hashCode)
      expect(ju.Objects.hashCode(1)).toEqual(1.hashCode)
    }

    it("should provide `hash`") {
      expect(ju.Objects.hash()).toEqual(ju.Arrays.hashCode(Array.empty[AnyRef]))
      expect(ju.Objects.hash(null)).toEqual(ju.Arrays.hashCode(Array[AnyRef](null)))
      expect(ju.Objects.hash("1")).toEqual(ju.Arrays.hashCode(Array[AnyRef]("1")))
      expect(ju.Objects.hash("1", "2")).toEqual(ju.Arrays.hashCode(Array[AnyRef]("1", "2")))
      expect(ju.Objects.hash("1", null)).toEqual(ju.Arrays.hashCode(Array[AnyRef]("1", null)))
    }

    it("should provide `toString`") {
      val obj = new Object
      expect(ju.Objects.toString(null)).toEqual("null")
      expect(ju.Objects.toString(null, "abc")).toEqual("abc")
      expect(ju.Objects.toString(obj)).toEqual(obj.toString)
      expect(ju.Objects.toString(obj, "abc")).toEqual(obj.toString)
      expect(ju.Objects.toString(1)).toEqual(1.toString)
      expect(ju.Objects.toString(1, "abc")).toEqual(1.toString)
    }

    it("should provide `compare`") {
      val cmp1: ju.Comparator[Int] = Ordering[Int]
      val cmp2: ju.Comparator[AnyRef] = new Ordering[AnyRef] {
        def compare(x: AnyRef, y: AnyRef): Int =
          x.hashCode.compareTo(y.hashCode)
      }
      expect(ju.Objects.compare(null, null, cmp2)).toEqual(0)
      expect(ju.Objects.compare(1, 1, cmp1)).toEqual(0)
      expect(ju.Objects.compare(2, 1, cmp1)).toBeGreaterThan(0)
      expect(ju.Objects.compare(1, 2, cmp1)).toBeLessThan(0)
    }

    it("should provide `requireNonNull`") {
      expectThrows[NullPointerException](ju.Objects.requireNonNull(null))
      expectThrows[NullPointerException](ju.Objects.requireNonNull(null, ""))
      expect(ju.Objects.requireNonNull("abc")).toEqual("abc")
      expect(ju.Objects.requireNonNull("abc", "")).toEqual("abc")
    }
  }
}
