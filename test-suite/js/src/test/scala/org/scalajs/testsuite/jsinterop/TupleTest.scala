/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.jsinterop

import scala.language.implicitConversions

import scala.scalajs.js

import org.junit.Assert._
import org.junit.Test

import org.scalajs.testsuite.utils.JSAssert._

class TupleTest {

  @Test def should_provide_an_equivalent_of_Scala_tuple(): Unit = {
    val obj = js.Tuple2(42, "foobar")

    assertEquals(42, obj._1)
    assertEquals("foobar", obj._2)
  }

  @Test def should_unapply_JS_tuple_in_destructuring_use_case(): Unit = {
    val obj = js.Tuple2(42, "foobar")
    val js.Tuple2(t1, t2) = obj

    val t1IsInt: Int = t1
    val t2IsString: String = t2
    assertEquals(42, t1IsInt)
    assertEquals("foobar", t2IsString)
  }

  @Test def should_unapply_JS_tuple_in_pattern_matching_position(): Unit = {
    val obj = js.Tuple2(42, "foobar")
    obj match {
      case js.Tuple2(2, _) =>
        fail("Not expected match")
      case js.Tuple2(t1, t2) =>
        val t1IsInt: Int = t1
        val t2IsString: String = t2
        assertEquals(42, t1IsInt)
        assertEquals("foobar", t2IsString)
    }
  }

  @Test def should_be_a_JS_array_instance(): Unit = {
    val obj = js.Tuple2(42, "foobar")

    assertTrue((obj: Any).isInstanceOf[js.Array[_]])
    assertJSArrayEquals(js.Array(42, "foobar"), obj.asInstanceOf[js.Array[Any]])
  }

  @Test def should_be_able_to_cast_from_a_JS_array_instance(): Unit = {
    val obj = js.Array[Any](42, "foobar").asInstanceOf[js.Tuple2[Int, String]]

    assertEquals(42, obj._1)
    assertEquals("foobar", obj._2)
  }

  @Test def should_convert_from_Scala_tuple(): Unit = {
    val obj: js.Tuple2[Int, String] = (42, "foobar")

    assertEquals(42, obj._1)
    assertEquals("foobar", obj._2)
  }

  @Test def should_convert_to_Scala_tuple(): Unit = {
    val obj: (Int, String) = js.Tuple2(42, "foobar")

    assertEquals(42, obj._1)
    assertEquals("foobar", obj._2)
  }

  // scalastyle:off line.size.limit

  @Test def should_support_tuple_of_2(): Unit = {
    val jsObj = js.Tuple2("1", 2)
    val scalaObj: (String, Int) = jsObj
    val t2IsInt: Int = js.Tuple2.unapply(jsObj).get._2

    assertEquals(2, jsObj._2)
    assertEquals(2, scalaObj._2)
    assertEquals(2, t2IsInt)
    assertTrue((jsObj: Any).isInstanceOf[js.Array[_]])
    assertJSArrayEquals(js.Array("1", 2), jsObj.asInstanceOf[js.Array[Any]])
  }

  @Test def should_support_tuple_of_3(): Unit = {
    val jsObj = js.Tuple3("1", "2", 3)
    val scalaObj: (String, String, Int) = jsObj
    val t3IsInt: Int = js.Tuple3.unapply(jsObj).get._3

    assertEquals(3, jsObj._3)
    assertEquals(3, scalaObj._3)
    assertEquals(3, t3IsInt)
    assertTrue((jsObj: Any).isInstanceOf[js.Array[_]])
    assertJSArrayEquals(js.Array("1", "2", 3), jsObj.asInstanceOf[js.Array[Any]])
  }

  @Test def should_support_tuple_of_4(): Unit = {
    val jsObj = js.Tuple4("1", "2", "3", 4)
    val scalaObj: (String, String, String, Int) = jsObj
    val t4IsInt: Int = js.Tuple4.unapply(jsObj).get._4

    assertEquals(4, jsObj._4)
    assertEquals(4, scalaObj._4)
    assertEquals(4, t4IsInt)
    assertTrue((jsObj: Any).isInstanceOf[js.Array[_]])
    assertJSArrayEquals(js.Array("1", "2", "3", 4), jsObj.asInstanceOf[js.Array[Any]])
  }

  @Test def should_support_tuple_of_5(): Unit = {
    val jsObj = js.Tuple5("1", "2", "3", "4", 5)
    val scalaObj: (String, String, String, String, Int) = jsObj
    val t5IsInt: Int = js.Tuple5.unapply(jsObj).get._5

    assertEquals(5, jsObj._5)
    assertEquals(5, scalaObj._5)
    assertEquals(5, t5IsInt)
    assertTrue((jsObj: Any).isInstanceOf[js.Array[_]])
    assertJSArrayEquals(js.Array("1", "2", "3", "4", 5), jsObj.asInstanceOf[js.Array[Any]])
  }

  @Test def should_support_tuple_of_6(): Unit = {
    val jsObj = js.Tuple6("1", "2", "3", "4", "5", 6)
    val scalaObj: (String, String, String, String, String, Int) = jsObj
    val t6IsInt: Int = js.Tuple6.unapply(jsObj).get._6

    assertEquals(6, jsObj._6)
    assertEquals(6, scalaObj._6)
    assertEquals(6, t6IsInt)
    assertTrue((jsObj: Any).isInstanceOf[js.Array[_]])
    assertJSArrayEquals(js.Array("1", "2", "3", "4", "5", 6), jsObj.asInstanceOf[js.Array[Any]])
  }

  @Test def should_support_tuple_of_7(): Unit = {
    val jsObj = js.Tuple7("1", "2", "3", "4", "5", "6", 7)
    val scalaObj: (String, String, String, String, String, String, Int) = jsObj
    val t7IsInt: Int = js.Tuple7.unapply(jsObj).get._7

    assertEquals(7, jsObj._7)
    assertEquals(7, scalaObj._7)
    assertEquals(7, t7IsInt)
    assertTrue((jsObj: Any).isInstanceOf[js.Array[_]])
    assertJSArrayEquals(js.Array("1", "2", "3", "4", "5", "6", 7), jsObj.asInstanceOf[js.Array[Any]])
  }

  @Test def should_support_tuple_of_8(): Unit = {
    val jsObj = js.Tuple8("1", "2", "3", "4", "5", "6", "7", 8)
    val scalaObj: (String, String, String, String, String, String, String, Int) = jsObj
    val t8IsInt: Int = js.Tuple8.unapply(jsObj).get._8

    assertEquals(8, jsObj._8)
    assertEquals(8, scalaObj._8)
    assertEquals(8, t8IsInt)
    assertTrue((jsObj: Any).isInstanceOf[js.Array[_]])
    assertJSArrayEquals(js.Array("1", "2", "3", "4", "5", "6", "7", 8), jsObj.asInstanceOf[js.Array[Any]])
  }

  @Test def should_support_tuple_of_9(): Unit = {
    val jsObj = js.Tuple9("1", "2", "3", "4", "5", "6", "7", "8", 9)
    val scalaObj: (String, String, String, String, String, String, String, String, Int) = jsObj
    val t9IsInt: Int = js.Tuple9.unapply(jsObj).get._9

    assertEquals(9, jsObj._9)
    assertEquals(9, scalaObj._9)
    assertEquals(9, t9IsInt)
    assertTrue((jsObj: Any).isInstanceOf[js.Array[_]])
    assertJSArrayEquals(js.Array("1", "2", "3", "4", "5", "6", "7", "8", 9), jsObj.asInstanceOf[js.Array[Any]])
  }

  @Test def should_support_tuple_of_10(): Unit = {
    val jsObj = js.Tuple10("1", "2", "3", "4", "5", "6", "7", "8", "9", 10)
    val scalaObj: (String, String, String, String, String, String, String, String, String, Int) = jsObj
    val t10IsInt: Int = js.Tuple10.unapply(jsObj).get._10

    assertEquals(10, jsObj._10)
    assertEquals(10, scalaObj._10)
    assertEquals(10, t10IsInt)
    assertTrue((jsObj: Any).isInstanceOf[js.Array[_]])
    assertJSArrayEquals(js.Array("1", "2", "3", "4", "5", "6", "7", "8", "9", 10), jsObj.asInstanceOf[js.Array[Any]])
  }

  @Test def should_support_tuple_of_11(): Unit = {
    val jsObj = js.Tuple11("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", 11)
    val scalaObj: (String, String, String, String, String, String, String, String, String, String, Int) = jsObj
    val t11IsInt: Int = js.Tuple11.unapply(jsObj).get._11

    assertEquals(11, jsObj._11)
    assertEquals(11, scalaObj._11)
    assertEquals(11, t11IsInt)
    assertTrue((jsObj: Any).isInstanceOf[js.Array[_]])
    assertJSArrayEquals(js.Array("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", 11), jsObj.asInstanceOf[js.Array[Any]])
  }

  @Test def should_support_tuple_of_12(): Unit = {
    val jsObj = js.Tuple12("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", 12)
    val scalaObj: (String, String, String, String, String, String, String, String, String, String, String, Int) = jsObj
    val t12IsInt: Int = js.Tuple12.unapply(jsObj).get._12

    assertEquals(12, jsObj._12)
    assertEquals(12, scalaObj._12)
    assertEquals(12, t12IsInt)
    assertTrue((jsObj: Any).isInstanceOf[js.Array[_]])
    assertJSArrayEquals(js.Array("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", 12), jsObj.asInstanceOf[js.Array[Any]])
  }

  @Test def should_support_tuple_of_13(): Unit = {
    val jsObj = js.Tuple13("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", 13)
    val scalaObj: (String, String, String, String, String, String, String, String, String, String, String, String, Int) = jsObj
    val t13IsInt: Int = js.Tuple13.unapply(jsObj).get._13

    assertEquals(13, jsObj._13)
    assertEquals(13, scalaObj._13)
    assertEquals(13, t13IsInt)
    assertTrue((jsObj: Any).isInstanceOf[js.Array[_]])
    assertJSArrayEquals(js.Array("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", 13), jsObj.asInstanceOf[js.Array[Any]])
  }

  @Test def should_support_tuple_of_14(): Unit = {
    val jsObj = js.Tuple14("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", 14)
    val scalaObj: (String, String, String, String, String, String, String, String, String, String, String, String, String, Int) = jsObj
    val t14IsInt: Int = js.Tuple14.unapply(jsObj).get._14

    assertEquals(14, jsObj._14)
    assertEquals(14, scalaObj._14)
    assertEquals(14, t14IsInt)
    assertTrue((jsObj: Any).isInstanceOf[js.Array[_]])
    assertJSArrayEquals(js.Array("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", 14), jsObj.asInstanceOf[js.Array[Any]])
  }

  @Test def should_support_tuple_of_15(): Unit = {
    val jsObj = js.Tuple15("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", 15)
    val scalaObj: (String, String, String, String, String, String, String, String, String, String, String, String, String, String, Int) = jsObj
    val t15IsInt: Int = js.Tuple15.unapply(jsObj).get._15

    assertEquals(15, jsObj._15)
    assertEquals(15, scalaObj._15)
    assertEquals(15, t15IsInt)
    assertTrue((jsObj: Any).isInstanceOf[js.Array[_]])
    assertJSArrayEquals(js.Array("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", 15), jsObj.asInstanceOf[js.Array[Any]])
  }

  @Test def should_support_tuple_of_16(): Unit = {
    val jsObj = js.Tuple16("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", 16)
    val scalaObj: (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, Int) = jsObj
    val t16IsInt: Int = js.Tuple16.unapply(jsObj).get._16

    assertEquals(16, jsObj._16)
    assertEquals(16, scalaObj._16)
    assertEquals(16, t16IsInt)
    assertTrue((jsObj: Any).isInstanceOf[js.Array[_]])
    assertJSArrayEquals(js.Array("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", 16), jsObj.asInstanceOf[js.Array[Any]])
  }

  @Test def should_support_tuple_of_17(): Unit = {
    val jsObj = js.Tuple17("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", 17)
    val scalaObj: (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, Int) = jsObj
    val t17IsInt: Int = js.Tuple17.unapply(jsObj).get._17

    assertEquals(17, jsObj._17)
    assertEquals(17, scalaObj._17)
    assertEquals(17, t17IsInt)
    assertTrue((jsObj: Any).isInstanceOf[js.Array[_]])
    assertJSArrayEquals(js.Array("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", 17), jsObj.asInstanceOf[js.Array[Any]])
  }

  @Test def should_support_tuple_of_18(): Unit = {
    val jsObj = js.Tuple18("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", 18)
    val scalaObj: (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, Int) = jsObj
    val t18IsInt: Int = js.Tuple18.unapply(jsObj).get._18

    assertEquals(18, jsObj._18)
    assertEquals(18, scalaObj._18)
    assertEquals(18, t18IsInt)
    assertTrue((jsObj: Any).isInstanceOf[js.Array[_]])
    assertJSArrayEquals(js.Array("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", 18), jsObj.asInstanceOf[js.Array[Any]])
  }

  @Test def should_support_tuple_of_19(): Unit = {
    val jsObj = js.Tuple19("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", 19)
    val scalaObj: (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, Int) = jsObj
    val t19IsInt: Int = js.Tuple19.unapply(jsObj).get._19

    assertEquals(19, jsObj._19)
    assertEquals(19, scalaObj._19)
    assertEquals(19, t19IsInt)
    assertTrue((jsObj: Any).isInstanceOf[js.Array[_]])
    assertJSArrayEquals(js.Array("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", 19), jsObj.asInstanceOf[js.Array[Any]])
  }

  @Test def should_support_tuple_of_20(): Unit = {
    val jsObj = js.Tuple20("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", 20)
    val scalaObj: (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, Int) = jsObj
    val t20IsInt: Int = js.Tuple20.unapply(jsObj).get._20

    assertEquals(20, jsObj._20)
    assertEquals(20, scalaObj._20)
    assertEquals(20, t20IsInt)
    assertTrue((jsObj: Any).isInstanceOf[js.Array[_]])
    assertJSArrayEquals(js.Array("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", 20), jsObj.asInstanceOf[js.Array[Any]])
  }

  @Test def should_support_tuple_of_21(): Unit = {
    val jsObj = js.Tuple21("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", 21)
    val scalaObj: (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, Int) = jsObj
    val t21IsInt: Int = js.Tuple21.unapply(jsObj).get._21

    assertEquals(21, jsObj._21)
    assertEquals(21, scalaObj._21)
    assertEquals(21, t21IsInt)
    assertJSArrayEquals(js.Array("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", 21), jsObj.asInstanceOf[js.Array[Any]])
  }

  @Test def should_support_tuple_of_22(): Unit = {
    val jsObj = js.Tuple22("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", 22)
    val scalaObj: (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, Int) = jsObj
    val t22IsInt: Int = js.Tuple22.unapply(jsObj).get._22

    assertEquals(22, jsObj._22)
    assertEquals(22, scalaObj._22)
    assertEquals(22, t22IsInt)
    assertTrue((jsObj: Any).isInstanceOf[js.Array[_]])
    assertJSArrayEquals(js.Array("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", 22), jsObj.asInstanceOf[js.Array[Any]])
  }

  // scalastyle:on line.size.limit

}
