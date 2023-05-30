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

package org.scalajs.testsuite.javalib.util

import java.nio.CharBuffer
import java.util.StringJoiner

import org.junit.Assert._
import org.junit.Assume._
import org.junit.Test

import org.scalajs.testsuite.utils.AssertThrows.assertThrows
import org.scalajs.testsuite.utils.Platform

class StringJoinerTest {
  import StringJoinerTest._

  @Test def testEmpty(): Unit = {
    assertJoinerResult("")(new StringJoiner(","))
    assertJoinerResult("[]")(new StringJoiner(";", "[", "]"))
    assertJoinerResult("--")(new StringJoiner(";").setEmptyValue("--"))
    assertJoinerResult("--")(new StringJoiner(";", "[", "]").setEmptyValue("--"))
  }

  @Test def testNonEmpty(): Unit = {
    assertJoinerResult("one") {
      new StringJoiner(",").add("one")
    }
    assertJoinerResult("one,two,three") {
      new StringJoiner(",").add("one").add("two").add("three")
    }
    assertJoinerResult("[one, two, three]") {
      new StringJoiner(", ", "[", "]").add("one").add("two").add("three")
    }
    assertJoinerResult("[one, null, three]") {
      new StringJoiner(", ", "[", "]").add("one").add(null).add("three")
    }
    assertJoinerResult("[one, two, three]") {
      new StringJoiner(", ", "[", "]").add("one").add(CharBuffer.wrap("two")).add("three")
    }
    assertJoinerResult("") {
      new StringJoiner(",").add("")
    }
    assertJoinerResult("one,") {
      new StringJoiner(",").add("one").add("")
    }
    assertJoinerResult(",two") {
      new StringJoiner(",").add("").add("two")
    }
    assertJoinerResult("one,,three") {
      new StringJoiner(",").add("one").add("").add("three")
    }

    assertJoinerResult("one") {
      new StringJoiner(",").setEmptyValue("--").add("one")
    }
    assertJoinerResult("one,two,three") {
      new StringJoiner(",").setEmptyValue("--").add("one").add("two").add("three")
    }
    assertJoinerResult("[one, two, three]") {
      new StringJoiner(", ", "[", "]").add("one").add("two").setEmptyValue("--").add("three")
    }
    assertJoinerResult("[one, two, three]") {
      new StringJoiner(", ", "[", "]").add("one").add("two").add("three").setEmptyValue("--")
    }
    assertJoinerResult("") {
      new StringJoiner(",").setEmptyValue("--").add("")
    }
    assertJoinerResult("one,") {
      new StringJoiner(",").setEmptyValue("--").add("one").add("")
    }
  }

  @Test def testMerge(): Unit = {
    val empty = new StringJoiner(";", "[", "]").setEmptyValue("--")
    val single = new StringJoiner(";", "[", "]").setEmptyValue("--").add("single")
    val multiple = new StringJoiner(";", "[", "]").setEmptyValue("--").add("a").add("b").add("c")
    val singleBlank = new StringJoiner(";", "[", "]").setEmptyValue("--").add("")

    assertJoinerResult("+++") {
      new StringJoiner(", ", "{", "}").merge(empty).setEmptyValue("+++")
    }
    assertJoinerResult("+++") {
      new StringJoiner(", ", "{", "}").setEmptyValue("+++").merge(empty)
    }
    assertJoinerResult("{}") {
      new StringJoiner(", ", "{", "}").merge(singleBlank).setEmptyValue("+++")
    }
    assertJoinerResult("{}") {
      new StringJoiner(", ", "{", "}").setEmptyValue("+++").merge(singleBlank)
    }
    assertJoinerResult("{one, two}") {
      new StringJoiner(", ", "{", "}").add("one").merge(empty).add("two")
    }
    assertJoinerResult("{one, single, two}") {
      new StringJoiner(", ", "{", "}").add("one").merge(single).add("two")
    }
    assertJoinerResult("{one, a;b;c, two}") {
      new StringJoiner(", ", "{", "}").add("one").merge(multiple).add("two")
    }
    assertJoinerResult("{one, , two}") {
      new StringJoiner(", ", "{", "}").add("one").merge(singleBlank).add("two")
    }
    assertJoinerResult("{single}") {
      new StringJoiner(", ", "{", "}").merge(single)
    }
    assertJoinerResult("{a;b;c}") {
      new StringJoiner(", ", "{", "}").merge(multiple)
    }
    assertJoinerResult("{}") {
      new StringJoiner(", ", "{", "}").merge(singleBlank)
    }
  }

  @Test def testState(): Unit = {
    val mutableCharSeq = CharBuffer.allocate(2).put(0, '?').put(1, '!')

    val joiner = new StringJoiner(mutableCharSeq, mutableCharSeq, mutableCharSeq)
    assertJoinerResult("?!?!")(joiner)
    joiner.setEmptyValue(mutableCharSeq)
    assertJoinerResult("?!")(joiner)

    mutableCharSeq.put(0, '-')
    assertJoinerResult("?!")(joiner) // the previously set emptyValue is not affected
    joiner.setEmptyValue(mutableCharSeq)
    assertJoinerResult("-!")(joiner)

    joiner.add("one")
    assertJoinerResult("?!one?!")(joiner) // the previously set prefix and suffix are not affected

    joiner.add("two")
    assertJoinerResult("?!one?!two?!")(joiner) // the previously set delimiter is not affected
  }

  @Test def testNPE(): Unit = {
    assumeTrue("requires compliant null pointers", Platform.hasCompliantNullPointers)

    @noinline
    def assertNPE[U](code: => U): Unit =
      assertThrows(classOf[NullPointerException], code)

    assertNPE(new StringJoiner(null))
    assertNPE(new StringJoiner(null, "[", "]"))
    assertNPE(new StringJoiner(",", null, "]"))
    assertNPE(new StringJoiner(",", "[", null))

    assertNPE(new StringJoiner(",").setEmptyValue(null))

    assertNPE(new StringJoiner(",").merge(null))
  }
}

object StringJoinerTest {
  def assertJoinerResult(expected: String)(joiner: StringJoiner): Unit = {
    assertEquals(expected, joiner.toString())
    assertEquals(expected.length(), joiner.length())
  }
}
