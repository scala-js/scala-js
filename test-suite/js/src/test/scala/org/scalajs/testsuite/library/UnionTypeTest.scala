/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.library

import scala.language.implicitConversions

import scala.collection.mutable

import scala.scalajs.js
import js.|

import org.scalajs.testsuite.Typechecking._

import org.junit.Assert._
import org.junit.Test

object UnionTypeTest {
  class Consumer[-A]
}

class UnionTypeTest {
  import UnionTypeTest._

  // js.| (postive)

  @Test def left_and_right(): Unit = {
    val x1: Int | String = 4
    assertEquals(4, x1)

    val x2: Int | String = "hello"
    assertEquals("hello", x2)
  }

  @Test def left_and_right_with_subtyping(): Unit = {
    val list = List(1, 2, 3)

    val x1: Seq[Int] | CharSequence = list
    assertTrue((x1: Any).isInstanceOf[List[_]])
    assertEquals(List(1, 2, 3), x1.asInstanceOf[List[_]])

    val x2: Seq[Int] | CharSequence = "hello"
    assertEquals("hello", x2)
  }

  @Test def three_types(): Unit = {
    val x1: Int | String | Boolean = 3
    assertEquals(3, x1)

    val x2: Int | String | Boolean = "hello"
    assertEquals("hello", x2)

    val x3: Int | String | Boolean = false
    assertEquals(false, x3)
  }

  @Test def upcast(): Unit = {
    val x1: List[Int] | String = "hello"
    val x2: Seq[Int] | CharSequence = x1
    assertEquals("hello", x2)
  }

  @Test def int_as_Double(): Unit = {
    val x1: Double | String = 3
    assertEquals(3, x1)
  }

  @Test def swap_base_types(): Unit = {
    val x1: Int | String = 3
    val x2: String | Int = x1
    assertEquals(3, x2)
  }

  @Test def permutations_for_3_base_types(): Unit = {
    val x: Int | String | Boolean = 3

    val x1: Int | Boolean | String = x
    val x2: String | Int | Boolean = x
    val x3: String | Boolean | Int = x
    val x4: Boolean | Int | String = x
    val x5: Boolean | String | Int = x

    assertEquals(3, x1)
    assertEquals(3, x2)
    assertEquals(3, x3)
    assertEquals(3, x4)
    assertEquals(3, x5)
  }

  @Test def permutations_of_2_base_types_to_3_base_types(): Unit = {
    val x1: Int | String = 3
    val x2: Int | Boolean = false
    val x3: Boolean | String = "hello"

    val y1: Int | String | Boolean = x1
    val y2: Int | String | Boolean = x2
    val y3: Int | String | Boolean = x3

    assertEquals(3, y1)
    assertEquals(false, y2)
    assertEquals("hello", y3)
  }

  @Test def partial_upper_bound(): Unit = {
    val x: Int | String | Boolean = "hello"

    val x1: AnyVal | String = x
    val x2: String | AnyVal = x

    assertEquals("hello", x1)
    assertEquals("hello", x2)

    /* Note: the *total* upper bound does not work without an explicit
     * `merge`, because the expected type is not an | type.
     */
  }

  @Test def merge(): Unit = {
    val x1: Int | Boolean = 4
    val y1: AnyVal = x1.merge
    assertEquals(4, y1.asInstanceOf[js.Any])

    val x2: String | java.nio.CharBuffer = "hello"
    val y2: CharSequence = x2.merge
    assertEquals("hello", y2.asInstanceOf[js.Any])

    val x3: Int | String | Boolean | java.nio.CharBuffer = "hello"
    val y3: CharSequence | AnyVal = x3.merge
    assertEquals("hello", y3.asInstanceOf[js.Any])

    val x4: List[Int] | Vector[Int] | mutable.Buffer[Int] = List(3, 5)
    val y4: Seq[Int] = x4.merge
    assertEquals(Seq(3, 5), y4)
  }

  @Test def js_UndefOr_A_or_B_inference(): Unit = {
    val a: String = "hello"

    assertEquals(a, a: Int | String)
    assertEquals(a, a: js.UndefOr[Int] | String)
    assertEquals(a, a: Int | js.UndefOr[String])
    assertEquals(a, a: js.UndefOr[Int] | js.UndefOr[String])
    assertEquals(a, a: js.UndefOr[js.UndefOr[Int]] | js.UndefOr[String])
    assertEquals(a, a: js.UndefOr[Int] | js.UndefOr[js.UndefOr[String]])

    assertEquals(a, a: js.UndefOr[Int | String])
    assertEquals(a, a: js.UndefOr[js.UndefOr[Int] | String])
    assertEquals(a, a: js.UndefOr[Int | js.UndefOr[String]])
    assertEquals(a, a: js.UndefOr[js.UndefOr[Int] | js.UndefOr[String]])
    assertEquals(a, a: js.UndefOr[js.UndefOr[js.UndefOr[Int]] | js.UndefOr[String]])
    assertEquals(a, a: js.UndefOr[js.UndefOr[Int] | js.UndefOr[js.UndefOr[String]]])

    assertEquals(a, a: js.UndefOr[String | Int])
    assertEquals(a, a: js.UndefOr[String | Int])
    assertEquals(a, a: js.UndefOr[js.UndefOr[String] | Int])
    assertEquals(a, a: js.UndefOr[String | js.UndefOr[Int]])
    assertEquals(a, a: js.UndefOr[js.UndefOr[String] | js.UndefOr[Int]])
    assertEquals(a, a: js.UndefOr[js.UndefOr[String] | js.UndefOr[js.UndefOr[Int]]])
    assertEquals(a, a: js.UndefOr[js.UndefOr[js.UndefOr[String]] | js.UndefOr[Int]])

    // Confirm that we're working with triple unions too

    assertEquals(a, a: js.UndefOr[String | Object | Int])
    assertEquals(a, a: js.UndefOr[String | Int | Object])
    assertEquals(a, a: js.UndefOr[Int | String | Object])
    assertEquals(a, a: js.UndefOr[Int | Object | String])
    assertEquals(a, a: js.UndefOr[Object | String | Int])
    assertEquals(a, a: js.UndefOr[Object | Object | String])

    assertEquals(a, a: js.UndefOr[js.UndefOr[String] | Object | Int])
    assertEquals(a, a: js.UndefOr[js.UndefOr[String] | Int | Object])
    assertEquals(a, a: js.UndefOr[js.UndefOr[Int] | String | Object])
    assertEquals(a, a: js.UndefOr[js.UndefOr[Int] | Object | String])
    assertEquals(a, a: js.UndefOr[js.UndefOr[Object] | String | Int])
    assertEquals(a, a: js.UndefOr[js.UndefOr[Object] | Object | String])
  }

  @Test def covariant_type_constructor(): Unit = {
    val a: List[Int] = List(5)

    assertSame(a, a: List[Int | String])
    assertSame(a, a: List[String | AnyVal])

    val b: Int | List[Int] = a

    assertSame(a, b: AnyVal | List[Int | String])
  }

  @Test def contravariant_type_constructor(): Unit = {
    val a: Consumer[CharSequence | Int] = new Consumer

    assertSame(a, a: Consumer[Int])
    assertSame(a, a: Consumer[String])
    assertSame(a, a: Consumer[Int | String])

    val b: Int | Consumer[CharSequence | Int] = a

    assertSame(a, b: Consumer[Int | String] | AnyVal)
  }

  // js.| (negative)

  /* Error messages vary a lot depending on the version of Scala, so we do
   * not test them.
   */

  @Test def neither_left_nor_right(): Unit = {
    typeError(
        "3: Boolean | String")
  }

  @Test def none_of_three_types(): Unit = {
    typeError(
        "3: Boolean | String | List[Int]")
  }

  @Test def wrong_type_parameter_on_left_or_right(): Unit = {
    typeError(
        "List(1, 2): List[String] | String")
    typeError(
        "List(1, 2): String | List[String]")
  }

  @Test def left_of_OR_type_is_not_a_subtype_of_rhs(): Unit = {
    typeError(
        "(1: Int | List[String]): String | List[String]")
  }

  @Test def right_of_OR_type_is_not_a_subtype_of_rhs(): Unit = {
    typeError(
        "(1: Int | List[String]): String | Int")
  }

  @Test def merge_with_an_incorrect_subtype(): Unit = {
    typeError(
        "(List(1, 2): List[Int] | Set[Int]).merge: Seq[Int]")
  }

  @Test def invariant_type_constructor(): Unit = {
    typeError(
        "(Array[Int]()): Array[Int | String]")
  }

  @Test def covariant_type_constructor_in_contravariant_pos(): Unit = {
    typeError(
        "(Nil: List[Int | String]): List[Int]")
  }

  @Test def contravariant_type_constructor_in_covariant_pos(): Unit = {
    typeError(
        "(new Consumer[Int]): Consumer[Int | String]")
  }
}
