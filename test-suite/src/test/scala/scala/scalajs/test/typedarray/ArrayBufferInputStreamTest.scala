/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.test
package typedarray

import javalib.CommonStreamsTests

import scala.scalajs.js.typedarray._

/** Tests for our implementation of java.io._ stream classes */
object ArrayBufferInputStreamTest extends JasmineTest with CommonStreamsTests {

  when("typedarray").
  describe("scala.scalajs.js.typedarray.ArrayBufferInputStream") {
    byteArrayInputStreamLikeTests(seq => new ArrayBufferInputStream(
        new Int8Array(seq).buffer))
  }

}
