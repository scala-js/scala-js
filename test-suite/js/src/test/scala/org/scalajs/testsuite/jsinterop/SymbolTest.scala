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

package org.scalajs.testsuite.jsinterop

import java.util.Objects

import scala.scalajs.js

import org.junit.Assert._
import org.junit.Assume._
import org.junit.{BeforeClass, Test}

import org.scalajs.testsuite.utils.AssertThrows.assertThrows
import org.scalajs.testsuite.utils.Platform._

object SymbolTest {
  @BeforeClass def assumeSymbolsAreSupported(): Unit = {
    assumeTrue("Assuming JavaScript symbols are supported",
        org.scalajs.testsuite.utils.Platform.jsSymbols)
  }
}

class SymbolTest {

  val namedSymbol = js.Symbol.forKey("namedsym")
  val opaqueSymbolWithDesc = js.Symbol("opaqueSymbolWithDesc")
  val opaqueSymbolWithoutDesc = js.Symbol()

  @Test def typeOf(): Unit = {
    assertEquals("symbol", js.typeOf(namedSymbol))
    assertEquals("symbol", js.typeOf(opaqueSymbolWithDesc))
    assertEquals("symbol", js.typeOf(opaqueSymbolWithoutDesc))
  }

  @Test def keyFor(): Unit = {
    assertEquals("namedsym", js.Symbol.keyFor(namedSymbol))
    assertEquals(js.undefined, js.Symbol.keyFor(opaqueSymbolWithDesc))
    assertEquals(js.undefined, js.Symbol.keyFor(opaqueSymbolWithoutDesc))
  }

  @Test def identity(): Unit = {
    assertSame(namedSymbol, js.Symbol.forKey("namedsym"))
    assertNotSame(namedSymbol, js.Symbol("namedsym"))
    assertNotSame(opaqueSymbolWithDesc, js.Symbol("opaqueSymbolWithDesc"))
    assertNotSame(opaqueSymbolWithoutDesc, js.Symbol())
  }

  @Test def testToString(): Unit = {
    assertEquals("Symbol(namedsym)", namedSymbol.toString())
    assertEquals("Symbol(opaqueSymbolWithDesc)", opaqueSymbolWithDesc.toString())
    assertEquals("Symbol()", opaqueSymbolWithoutDesc.toString())
  }

  @Test def stringValueOf_Issue5407(): Unit = {
    assertEquals("Symbol(namedsym)", String.valueOf(namedSymbol))
    assertEquals("Symbol(opaqueSymbolWithDesc)", String.valueOf(opaqueSymbolWithDesc))
    assertEquals("Symbol()", String.valueOf(opaqueSymbolWithoutDesc))
  }

  @Test def objectsToString_Issue5407(): Unit = {
    assertEquals("Symbol(namedsym)", Objects.toString(namedSymbol))
    assertEquals("Symbol(opaqueSymbolWithDesc)", Objects.toString(opaqueSymbolWithDesc))
    assertEquals("Symbol()", Objects.toString(opaqueSymbolWithoutDesc))
  }

  @Test def stringConcatTypeError(): Unit = {
    assumeFalse("GCC wrongly optimizes out string concat in statement position",
        usesClosureCompiler)

    @noinline def inStringConcat(x: Any): String = ":" + x

    assertThrowsTypeError(inStringConcat(namedSymbol))
    assertThrowsTypeError(inStringConcat(opaqueSymbolWithDesc))
    assertThrowsTypeError(inStringConcat(opaqueSymbolWithoutDesc))

    @inline def inStringConcatInline(x: Any): String = ":" + x

    assertThrowsTypeError(inStringConcatInline(namedSymbol))
    assertThrowsTypeError(inStringConcatInline(opaqueSymbolWithDesc))
    assertThrowsTypeError(inStringConcatInline(opaqueSymbolWithoutDesc))
  }

  @Test def stringInterpolatorTypeError(): Unit = {
    assumeFalse("GCC wrongly optimizes out string concat in statement position",
        usesClosureCompiler)

    @noinline def inStringInterp(x: Any): String = s":$x"

    assertThrowsTypeError(inStringInterp(namedSymbol))
    assertThrowsTypeError(inStringInterp(opaqueSymbolWithDesc))
    assertThrowsTypeError(inStringInterp(opaqueSymbolWithoutDesc))

    @inline def inStringInterpInline(x: Any): String = s":$x"

    assertThrowsTypeError(inStringInterpInline(namedSymbol))
    assertThrowsTypeError(inStringInterpInline(opaqueSymbolWithDesc))
    assertThrowsTypeError(inStringInterpInline(opaqueSymbolWithoutDesc))
  }

  @noinline
  private def assertThrowsTypeError(op: => Unit): Unit = {
    val th = assertThrows(classOf[js.JavaScriptException], op)
    assertTrue(th.toString(), th.exception.isInstanceOf[js.TypeError])
  }

  @Test def wellKnownSymbolIterator(): Unit = {
    val sym = js.Symbol.iterator
    assertEquals("symbol", js.typeOf(sym))
    assertEquals(js.undefined, js.Symbol.keyFor(sym))
    assertEquals("Symbol(Symbol.iterator)", sym.toString())
  }

}
