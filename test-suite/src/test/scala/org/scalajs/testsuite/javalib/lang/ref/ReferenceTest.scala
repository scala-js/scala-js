/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.lang.ref

import org.scalajs.jasminetest.JasmineTest

object ReferenceTest extends JasmineTest {

  describe("java.land.ref.Reference") {

    it("Should have all the normal operations") {
      val s = "string"
      val ref = new java.lang.ref.WeakReference(s)
      expect(ref.get).toEqual(s)
      expect(ref.enqueue).toEqual(false)
      expect(ref.isEnqueued).toEqual(false)
      ref.clear
      // can't use `expect` because it tries to be clever and .toString things,
      // which makes it blow up when you pass in null
      assert(ref.get == null)
    }
  }
}
