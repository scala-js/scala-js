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

import scala.scalajs.js

import org.junit.Assert._
import org.junit.Assume._
import org.junit.{BeforeClass, Test}

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

  @Test def wellKnownSymbolIterator(): Unit = {
    val sym = js.Symbol.iterator
    assertEquals("symbol", js.typeOf(sym))
    assertEquals(js.undefined, js.Symbol.keyFor(sym))
    assertEquals("Symbol(Symbol.iterator)", sym.toString())
  }

}
