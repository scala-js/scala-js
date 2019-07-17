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

import org.junit.Assert._
import org.junit.Assume._
import org.junit.Test
import org.scalajs.testsuite.utils.Platform._

import scala.scalajs.js

/** Additional tests for access to the local scope.
  *
  * If moved to testSuite, those tests will cause the test suite to be emitted
  * significantly more slowly on the first pass, because a "dangerous global
  * ref" is accessed.
  */
class LocalScopeTestEx {
  @Test def local_synthetic_var_should_not_name_clash(): Unit = {
    assumeTrue("Assuming execution in Node.js", executingInNodeJS)

    def f(name: String): String = {
      var idx = name.length - 1
      var buf: String = ""
      var `jsx$1` = ""
      while (idx >= 0 && name.charAt(idx) != '$') {
        idx -= 1
        `jsx$1` += name
        buf += `jsx$1`
      }
      buf
    }
    assertEquals("foofoofoofoofoofoo", f("foo"))

    def f2(name: String): String = {
      var idx = name.length - 1
      var buf: String = ""
      while (idx >= 0 && name.charAt(idx) != '$') {
        idx -= 1
        var `jsx$1` = "" + name
        var idx2 = name.length - 2
        while (idx2 >= 0 && name.charAt(idx2) != '$') {
          idx2 -= 1
          var `jsx$2` = "" + name
          buf += `jsx$1` + `jsx$2`
        }
      }
      buf
    }
    assertEquals("barbarbarbarbarbarbarbarbarbarbarbar", f2("bar"))
  }
}


