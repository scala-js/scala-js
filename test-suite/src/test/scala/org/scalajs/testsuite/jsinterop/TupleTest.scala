/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.jsinterop

import org.scalajs.jasminetest.JasmineTest

import scala.scalajs.js

object TupleTest extends JasmineTest {

  describe("scala.scalajs.js.Tuple") {

    it("should provide an equivalent of Scala tuple") {
      val obj = js.Tuple2(42, "foobar")

      expect(obj._1).toEqual(42)
      expect(obj._2).toEqual("foobar")
    }

    it("should unapply JS tuple in destructuring use case") {
      val obj = js.Tuple2(42, "foobar")
      val js.Tuple2(t1, t2) = obj

      val t1IsInt: Int = t1
      val t2IsString: String = t2
      expect(t1IsInt).toEqual(42)
      expect(t2IsString).toEqual("foobar")
    }

    it("should unapply JS tuple in pattern matching position") {
      val obj = js.Tuple2(42, "foobar")
      obj match {
        case js.Tuple2(2, _) =>
          fail("Not expected match")
        case js.Tuple2(t1, t2) =>
          val t1IsInt: Int = t1
          val t2IsString: String = t2
          expect(t1IsInt).toEqual(42)
          expect(t2IsString).toEqual("foobar")
      }
    }

    it("should be a JS array instance") {
      val obj = js.Tuple2(42, "foobar")

      expect((obj: Any).isInstanceOf[js.Array[_]]).toBeTruthy
      expect(obj).toEqual(js.Array(42, "foobar"))
    }

    it("should be able to cast from a JS array instance") {
      val obj = js.Array[Any](42, "foobar").asInstanceOf[js.Tuple2[Int, String]]

      expect(obj._1).toEqual(42)
      expect(obj._2).toEqual("foobar")
    }

    it("should convert from Scala tuple") {
      val obj: js.Tuple2[Int, String] = (42, "foobar")

      expect(obj._1).toEqual(42)
      expect(obj._2).toEqual("foobar")
    }

    it("should convert to Scala tuple") {
      val obj: (Int, String) = js.Tuple2(42, "foobar")

      expect(obj._1).toEqual(42)
      expect(obj._2).toEqual("foobar")
    }

    // scalastyle:off line.size.limit

    it("should support tuple of 2") {
      val jsObj = js.Tuple2("1", 2)
      val scalaObj: (String, Int) = jsObj
      val t2IsInt: Int = js.Tuple2.unapply(jsObj).get._2

      expect(jsObj._2).toEqual(2)
      expect(scalaObj._2).toEqual(2)
      expect(t2IsInt).toEqual(2)
      expect(jsObj).toEqual(js.Array("1", 2))
    }

    it("should support tuple of 3") {
      val jsObj = js.Tuple3("1", "2", 3)
      val scalaObj: (String, String, Int) = jsObj
      val t3IsInt: Int = js.Tuple3.unapply(jsObj).get._3

      expect(jsObj._3).toEqual(3)
      expect(scalaObj._3).toEqual(3)
      expect(t3IsInt).toEqual(3)
      expect(jsObj).toEqual(js.Array("1", "2", 3))
    }

    it("should support tuple of 4") {
      val jsObj = js.Tuple4("1", "2", "3", 4)
      val scalaObj: (String, String, String, Int) = jsObj
      val t4IsInt: Int = js.Tuple4.unapply(jsObj).get._4

      expect(jsObj._4).toEqual(4)
      expect(scalaObj._4).toEqual(4)
      expect(t4IsInt).toEqual(4)
      expect(jsObj).toEqual(js.Array("1", "2", "3", 4))
    }

    it("should support tuple of 5") {
      val jsObj = js.Tuple5("1", "2", "3", "4", 5)
      val scalaObj: (String, String, String, String, Int) = jsObj
      val t5IsInt: Int = js.Tuple5.unapply(jsObj).get._5

      expect(jsObj._5).toEqual(5)
      expect(scalaObj._5).toEqual(5)
      expect(t5IsInt).toEqual(5)
      expect(jsObj).toEqual(js.Array("1", "2", "3", "4", 5))
    }

    it("should support tuple of 6") {
      val jsObj = js.Tuple6("1", "2", "3", "4", "5", 6)
      val scalaObj: (String, String, String, String, String, Int) = jsObj
      val t6IsInt: Int = js.Tuple6.unapply(jsObj).get._6

      expect(jsObj._6).toEqual(6)
      expect(scalaObj._6).toEqual(6)
      expect(t6IsInt).toEqual(6)
      expect(jsObj).toEqual(js.Array("1", "2", "3", "4", "5", 6))
    }

    it("should support tuple of 7") {
      val jsObj = js.Tuple7("1", "2", "3", "4", "5", "6", 7)
      val scalaObj: (String, String, String, String, String, String, Int) = jsObj
      val t7IsInt: Int = js.Tuple7.unapply(jsObj).get._7

      expect(jsObj._7).toEqual(7)
      expect(scalaObj._7).toEqual(7)
      expect(t7IsInt).toEqual(7)
      expect(jsObj).toEqual(js.Array("1", "2", "3", "4", "5", "6", 7))
    }

    it("should support tuple of 8") {
      val jsObj = js.Tuple8("1", "2", "3", "4", "5", "6", "7", 8)
      val scalaObj: (String, String, String, String, String, String, String, Int) = jsObj
      val t8IsInt: Int = js.Tuple8.unapply(jsObj).get._8

      expect(jsObj._8).toEqual(8)
      expect(scalaObj._8).toEqual(8)
      expect(t8IsInt).toEqual(8)
      expect(jsObj).toEqual(js.Array("1", "2", "3", "4", "5", "6", "7", 8))
    }

    it("should support tuple of 9") {
      val jsObj = js.Tuple9("1", "2", "3", "4", "5", "6", "7", "8", 9)
      val scalaObj: (String, String, String, String, String, String, String, String, Int) = jsObj
      val t9IsInt: Int = js.Tuple9.unapply(jsObj).get._9

      expect(jsObj._9).toEqual(9)
      expect(scalaObj._9).toEqual(9)
      expect(t9IsInt).toEqual(9)
      expect(jsObj).toEqual(js.Array("1", "2", "3", "4", "5", "6", "7", "8", 9))
    }

    it("should support tuple of 10") {
      val jsObj = js.Tuple10("1", "2", "3", "4", "5", "6", "7", "8", "9", 10)
      val scalaObj: (String, String, String, String, String, String, String, String, String, Int) = jsObj
      val t10IsInt: Int = js.Tuple10.unapply(jsObj).get._10

      expect(jsObj._10).toEqual(10)
      expect(scalaObj._10).toEqual(10)
      expect(t10IsInt).toEqual(10)
      expect(jsObj).toEqual(js.Array("1", "2", "3", "4", "5", "6", "7", "8", "9", 10))
    }

    it("should support tuple of 11") {
      val jsObj = js.Tuple11("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", 11)
      val scalaObj: (String, String, String, String, String, String, String, String, String, String, Int) = jsObj
      val t11IsInt: Int = js.Tuple11.unapply(jsObj).get._11

      expect(jsObj._11).toEqual(11)
      expect(scalaObj._11).toEqual(11)
      expect(t11IsInt).toEqual(11)
      expect(jsObj).toEqual(js.Array("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", 11))
    }

    it("should support tuple of 12") {
      val jsObj = js.Tuple12("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", 12)
      val scalaObj: (String, String, String, String, String, String, String, String, String, String, String, Int) = jsObj
      val t12IsInt: Int = js.Tuple12.unapply(jsObj).get._12

      expect(jsObj._12).toEqual(12)
      expect(scalaObj._12).toEqual(12)
      expect(t12IsInt).toEqual(12)
      expect(jsObj).toEqual(js.Array("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", 12))
    }

    it("should support tuple of 13") {
      val jsObj = js.Tuple13("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", 13)
      val scalaObj: (String, String, String, String, String, String, String, String, String, String, String, String, Int) = jsObj
      val t13IsInt: Int = js.Tuple13.unapply(jsObj).get._13

      expect(jsObj._13).toEqual(13)
      expect(scalaObj._13).toEqual(13)
      expect(t13IsInt).toEqual(13)
      expect(jsObj).toEqual(js.Array("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", 13))
    }

    it("should support tuple of 14") {
      val jsObj = js.Tuple14("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", 14)
      val scalaObj: (String, String, String, String, String, String, String, String, String, String, String, String, String, Int) = jsObj
      val t14IsInt: Int = js.Tuple14.unapply(jsObj).get._14

      expect(jsObj._14).toEqual(14)
      expect(scalaObj._14).toEqual(14)
      expect(t14IsInt).toEqual(14)
      expect(jsObj).toEqual(js.Array("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", 14))
    }

    it("should support tuple of 15") {
      val jsObj = js.Tuple15("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", 15)
      val scalaObj: (String, String, String, String, String, String, String, String, String, String, String, String, String, String, Int) = jsObj
      val t15IsInt: Int = js.Tuple15.unapply(jsObj).get._15

      expect(jsObj._15).toEqual(15)
      expect(scalaObj._15).toEqual(15)
      expect(t15IsInt).toEqual(15)
      expect(jsObj).toEqual(js.Array("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", 15))
    }

    it("should support tuple of 16") {
      val jsObj = js.Tuple16("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", 16)
      val scalaObj: (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, Int) = jsObj
      val t16IsInt: Int = js.Tuple16.unapply(jsObj).get._16

      expect(jsObj._16).toEqual(16)
      expect(scalaObj._16).toEqual(16)
      expect(t16IsInt).toEqual(16)
      expect(jsObj).toEqual(js.Array("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", 16))
    }

    it("should support tuple of 17") {
      val jsObj = js.Tuple17("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", 17)
      val scalaObj: (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, Int) = jsObj
      val t17IsInt: Int = js.Tuple17.unapply(jsObj).get._17

      expect(jsObj._17).toEqual(17)
      expect(scalaObj._17).toEqual(17)
      expect(t17IsInt).toEqual(17)
      expect(jsObj).toEqual(js.Array("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", 17))
    }

    it("should support tuple of 18") {
      val jsObj = js.Tuple18("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", 18)
      val scalaObj: (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, Int) = jsObj
      val t18IsInt: Int = js.Tuple18.unapply(jsObj).get._18

      expect(jsObj._18).toEqual(18)
      expect(scalaObj._18).toEqual(18)
      expect(t18IsInt).toEqual(18)
      expect(jsObj).toEqual(js.Array("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", 18))
    }

    it("should support tuple of 19") {
      val jsObj = js.Tuple19("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", 19)
      val scalaObj: (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, Int) = jsObj
      val t19IsInt: Int = js.Tuple19.unapply(jsObj).get._19

      expect(jsObj._19).toEqual(19)
      expect(scalaObj._19).toEqual(19)
      expect(t19IsInt).toEqual(19)
      expect(jsObj).toEqual(js.Array("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", 19))
    }

    it("should support tuple of 20") {
      val jsObj = js.Tuple20("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", 20)
      val scalaObj: (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, Int) = jsObj
      val t20IsInt: Int = js.Tuple20.unapply(jsObj).get._20

      expect(jsObj._20).toEqual(20)
      expect(scalaObj._20).toEqual(20)
      expect(t20IsInt).toEqual(20)
      expect(jsObj).toEqual(js.Array("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", 20))
    }

    it("should support tuple of 21") {
      val jsObj = js.Tuple21("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", 21)
      val scalaObj: (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, Int) = jsObj
      val t21IsInt: Int = js.Tuple21.unapply(jsObj).get._21

      expect(jsObj._21).toEqual(21)
      expect(scalaObj._21).toEqual(21)
      expect(t21IsInt).toEqual(21)
      expect(jsObj).toEqual(js.Array("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", 21))
    }

    it("should support tuple of 22") {
      val jsObj = js.Tuple22("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", 22)
      val scalaObj: (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, Int) = jsObj
      val t22IsInt: Int = js.Tuple22.unapply(jsObj).get._22

      expect(jsObj._22).toEqual(22)
      expect(scalaObj._22).toEqual(22)
      expect(t22IsInt).toEqual(22)
      expect(jsObj).toEqual(js.Array("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", 22))
    }

    // scalastyle:on line.size.limit

  }
}
