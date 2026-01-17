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

import java.util.StringTokenizer

import org.junit.Assert._
import org.junit.Test

import org.scalajs.testsuite.utils.AssertThrows.assertThrows

class StringTokenizerTest {
  import StringTokenizerTest.assertTokenizerResult

  @Test def ctorWithDelim(): Unit = {
    assertTokenizerResult("This", "is", "a", "test", "String") {
      new StringTokenizer(":This:is:a:test:String:", ":")
    }
  }

  @Test def ctorWithDelimWithReturnDelims(): Unit = {
    assertTokenizerResult(":", "This", ":", "is", ":", "a", ":", "test", ":", "String", ":") {
      new StringTokenizer(":This:is:a:test:String:", ":", true)
    }
  }

  @Test def defaultDelimiters(): Unit = {
    assertTokenizerResult("This", "is", "a", "test", "String") {
      new StringTokenizer(" This\tis\na\rtest\fString ")
    }
  }

  @Test def countTokens(): Unit = {
    val st = new StringTokenizer("This is a test String")
    assertEquals(5, st.countTokens())
  }

  @Test def countTokensWithReturnDelims(): Unit = {
    val st = new StringTokenizer("This is a test String", " ", true)
    assertEquals(9, st.countTokens())
  }

  @Test def ctorEmptyString(): Unit = {
    val st = new StringTokenizer("")
    assertFalse(st.hasMoreTokens())
    assertFalse(st.hasMoreElements())
    assertThrows(classOf[NoSuchElementException], st.nextToken())
    assertThrows(classOf[NoSuchElementException], st.nextElement())
  }

  @Test def ctorNoDelimitersInString(): Unit = {
    assertTokenizerResult("ThisisatestString") {
      new StringTokenizer("ThisisatestString")
    }

    assertTokenizerResult("ThisisatestString") {
      new StringTokenizer("ThisisatestString", ":", true)
    }
  }

  @Test def nextTokenWithNewDelim(): Unit = {
    val st = new StringTokenizer("ab;cd;:", ";")
    assertEquals("ab", st.nextToken())
    assertEquals("cd", st.nextToken())
    assertTrue("hasMoreTokens returned false", st.hasMoreTokens())
    assertEquals(";", st.nextToken(":"))
    assertFalse("hasMoreTokens returned true", st.hasMoreTokens())
  }

  @Test def consecutiveReturnDelimsFalse(): Unit = {
    assertTokenizerResult("This", "is", "a", "test", "String") {
      new StringTokenizer("::This::is::a::test::String::", ":")
    }
  }

  @Test def consecutiveReturnDelimsTrue(): Unit = {
    assertTokenizerResult(":", ":", "This", ":", ":", "is",
        ":", ":", "a", ":", ":", "test", ":", ":", "String", ":", ":") {
      new StringTokenizer("::This::is::a::test::String::", ":", true)
    }
  }
}

object StringTokenizerTest {
  private def assertTokenizerResult(expected: String*)(makeTokenizer: => StringTokenizer): Unit = {
    assertElementResult(expected: _*)(makeTokenizer)
    assertTokenResult(expected: _*)(makeTokenizer)
  }

  private[this] def assertElementResult(expected: String*)(tokenizer: StringTokenizer): Unit = {
    assertTokenizerResultImpl(_.countTokens(), _.hasMoreElements(), _.nextElement())(expected: _*)(
        tokenizer)
  }

  private[this] def assertTokenResult(expected: String*)(tokenizer: StringTokenizer): Unit = {
    assertTokenizerResultImpl(_.countTokens(), _.hasMoreTokens(), _.nextToken())(expected: _*)(
        tokenizer)
  }

  private[this] def assertTokenizerResultImpl[T](getCount: StringTokenizer => Int,
      hasMore: StringTokenizer => Boolean,
      getNext: StringTokenizer => T)(expected: T*)(tokenizer: StringTokenizer): Unit = {

    assertEquals(expected.size, getCount(tokenizer))

    for (elem <- expected) {
      assertTrue(s"expected more tokens for $elem", hasMore(tokenizer))
      assertEquals(elem, tokenizer.nextToken())
    }

    assertFalse(hasMore(tokenizer))
    assertThrows(classOf[NoSuchElementException], getNext(tokenizer))
  }
}
