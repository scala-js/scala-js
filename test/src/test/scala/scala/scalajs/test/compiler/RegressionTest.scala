/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.test
package compiler

import scala.scalajs.js
import scala.scalajs.test.JasmineTest

object RegressionTest extends JasmineTest {

  describe("Scala.js compiler regression tests") {

    it("Wrong division conversion (7 / 2.0) - #18") {
      val div = 7 / 2.0
      expect(div).toEqual(3.5)
      expect(div.getClass.getName).toEqual("double")

      val mod = 7 % 2.0
      expect(mod).toEqual(1.0)
      expect(mod.getClass.getName).toEqual("double")
    }

    it("js.String + js.String is ambiguous - #20") {
      val a: js.String = "a"
      val b: js.String = "b"
      val c: js.String = a + b
      expect(c).toEqual("ab")
    }

    it("Abort with some pattern match guards - #22") {
      object PatternMatchGuards {
        def go(f: Int => Int) = f(1)
        def main(): Unit = {
          go {
            case x if false => x
          }
        }
      }
      // Nothing to check
    }

    it("Bad encoding for characters spanning 2 UTF-16 chars - #23") {
      val str = "A∀\uD835\uDCAB"
      var s: String = ""
      for (c <- str) {
        val code: Int = c
        s = s + code + " "
      }
      expect(s).toEqual("65 8704 55349 56491 ")
    }

    it("String concatenation with null - #26") {
      val x: Object = null
      expect(x + "check").toEqual("nullcheck")
    }

    class Bug66A(s: String, e: Object) {
      def this(e: Object) = this("", e)
      def this(s: String) = this(s, "")
    }
    class Bug66B(s: String, e: Object) extends Bug66A(s)

    it("should emit static calls when forwarding to another constructor - #66") {
      new Bug66B("", "")
    }

    it("should not swallow Unit expressions when converting to js.Undefined - #83") {
      var effectHappened = false
      def doEffect(): Unit = effectHappened = true
      def f(): js.Undefined = doEffect()
      f()
      expect(effectHappened).toBeTruthy
    }
    
    it("should correctly call subSequence on non-string CharSequences - #55") {
      val arr: CharSequence = Array('a','b','c','d')
      val ss = arr.subSequence(2,3)
      expect(ss.length()).toEqual(1)
      expect(ss.charAt(0)).toEqual('c')
    }

    it("should correctly concat primitive values to strings - #113") {
      expect(4 + "foo").toEqual("4foo")
      expect('a' + "foo").toEqual("afoo")
    }

    it("should resolve overloads on scala.Function.apply when converting to js.Function - #125") {
      class Fct extends Function1[Int,Any] {
        def apply(n: Int) = n
      }

      val scalaFunction = new Fct
      val jsFunction: js.Any = scalaFunction
      val thisFunction: js.ThisFunction = scalaFunction
    }
  }
}
