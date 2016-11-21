/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2017, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.jsinterop

import scala.scalajs.js

import org.junit.Assert._
import org.junit.Assume._
import org.junit.{BeforeClass, Test}

object SymbolTest {
  @BeforeClass def assumeSymbolsAreSupported(): Unit = {
    assumeTrue("Assuming JavaScript symbols are supported",
        org.scalajs.testsuite.utils.Platform.areJSSymbolsSupported)
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

  @Test def wellKnownSymbolIterator(): Unit = {
    val sym = js.Symbol.iterator
    assertEquals("symbol", js.typeOf(sym))
    assertEquals(js.undefined, js.Symbol.keyFor(sym))
    assertEquals("Symbol(Symbol.iterator)", sym.toString())
  }

}
