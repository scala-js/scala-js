/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.scalalib

import org.scalajs.jasminetest.JasmineTest

import scala.reflect.NameTransformer

object NameTransformerTest extends JasmineTest {

  describe("scala.reflect.NameTransformer") {

    it("decode - #1602") {
      /* Mostly to make sure it links.
       * We trust the Scala implementation for correctness. And if it isn't,
       * well, behaving the same as Scala is the correct thing do for us
       * anyway.
       */
      expect(NameTransformer.decode("$plus")).toEqual("+")
      expect(NameTransformer.decode("ab$plus")).toEqual("ab+")
      expect(NameTransformer.decode("$minus")).toEqual("-")
      expect(NameTransformer.decode("$plusx$minusy")).toEqual("+x-y")
      expect(NameTransformer.decode("$plus$minus")).toEqual("+-")
    }

  }

}
