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
import scala.scalajs.js.annotation.JSName
import scala.scalajs.js.annotation.JSBracketAccess

/*
 * Based on examples in:
 * http://lampwww.epfl.ch/~doeraene/scala-js/doc/js-interoperability.html
 */
object InteroperabilityTest extends JasmineTest {

  describe("JavaScript interoperability") {

    it("should support backquotes to escape Scala fields") {
      trait InteroperabilityTestFieldEscape extends js.Object {
        var `def`: js.Number
        def `val`(): js.Number = ???
        def `val`(n: js.Number): js.Number = ???
      }

      val obj = js.eval("""
        var interoperabilityTestFieldEscape = {
          def: 0,
          val: function(x) { if (x) this.def = x; return this.def; }
        };
        interoperabilityTestFieldEscape;
      """).asInstanceOf[InteroperabilityTestFieldEscape]

      obj.`def` = 7357
      expect(obj.`def`).toEqual(7357)
      expect(obj.`val`()).toEqual(7357)
      expect(obj.`val`(42)).toEqual(42)
    }

    it("should support @JSName to specify the JavaScript name for fields") {
      trait InteroperabilityTestJSName extends js.Object {
        @JSName("val")
        def value(): js.Number = ???
        @JSName("val")
        def value(n: js.Number): js.Number = ???
      }

      val obj = js.eval("""
        var interoperabilityTestJSName = {
          def: 42,
          val: function(x) { if (x) this.def = x; return this.def; }
        };
        interoperabilityTestJSName;
      """).asInstanceOf[InteroperabilityTestJSName]

      expect(obj.value()).toEqual(42)
      expect(obj.value(7357)).toEqual(7357)
    }

    it("should support @JSBracketAccess to specify access using []-subscription") {
      trait InteroperabilityTestJSBracketAccess extends js.Object {
        @JSBracketAccess
        def apply(index: js.Number): js.Number = ???
        @JSBracketAccess
        def update(index: js.Number, v: js.Number): Unit = ???
      }

      val obj = js.eval("""
        var interoperabilityTestJSBracketAccess = [ 0, 1, 7357 ];
        interoperabilityTestJSBracketAccess;
      """).asInstanceOf[InteroperabilityTestJSBracketAccess]

      expect(obj(2)).toEqual(7357)
      obj(2) = 42
      expect(obj(2)).toEqual(42)
    }

    it("should allow instanciation of JS classes inheriting from js.Object") {
      js.eval("""
        var InteroperabilityTestInherit = {
          Pattern: function(x) {
            this.field = 42;
            this.method = function() {
              return '42';
            };
            this.getConstructorParam = function() {
              return x;
            };
          }
      }
      """)

      val obj = new InteroperabilityTestPattern("Scala.js")
      expect(obj.field).toEqual(42)
      expect(obj.method).toEqual("42")
      expect(obj.getConstructorParam).toEqual("Scala.js")
    }

    it("should acces top-level JS objects via Scala objects inheriting from js.Object") {
      js.eval("""
        var InteroperabilityTestTopLevelObject = function(value) {
          return {
            value: value,
            valueAsInt: function() {
              return parseInt(value);
            }
          };
    	}
      """)

      // Use alias for convenience: see end of file for definition
      val TopLevel = InteroperabilityTestTopLevel

      val obj = TopLevel("7357")
      expect(obj.value).toEqual("7357")
      expect(obj.valueAsInt).toEqual(7357)
    }

    it("should acces top-level JS objects via Scala object inheriting from js.GlobalScope") {
      js.eval("""
        var interoperabilityTestGlobalScopeValue = "7357";
        var interoperabilityTestGlobalScopeValueAsInt = function() {
          return parseInt(interoperabilityTestGlobalScopeValue);
        };
      """)

      // Use alias for convenience: see end of file for definition
      val Global = InteroperabilityTestGlobalScope

      expect(Global.interoperabilityTestGlobalScopeValue).toEqual("7357")
      expect(Global.interoperabilityTestGlobalScopeValueAsInt).toEqual(7357)

      Global.interoperabilityTestGlobalScopeValue = "42"
      expect(Global.interoperabilityTestGlobalScopeValueAsInt).toEqual(42)
    }

    it("should protect receiver of raw JS apply if it's a select") {
      trait InteroperabilityTestRawReceiver extends js.Object {
        val check: js.Function1[js.Number, js.Number] = ???
      }

      val rawReceiver = js.eval("""
        var interoperabilityTestRawReceiver = {
          member: 0xbad,
          check: function(raw) { return this.member ? this.member : raw; }
        };
        interoperabilityTestRawReceiver;
      """).asInstanceOf[InteroperabilityTestRawReceiver]

      expect(rawReceiver.check(7357)).toEqual(7357)

      val check = rawReceiver.check
      expect(check(0x600d)).toEqual(0x600d)
    }

  }
}

/*
 * Helper classes, traits and objects defined here since they cannot be nested.
 */

@JSName("InteroperabilityTestInherit.Pattern")
class InteroperabilityTestPattern protected () extends js.Object {
  def this(pattern: js.String) = this()
  val field: js.Number = ???
  def method(): js.String = ???
  def getConstructorParam(): js.String = ???
}

trait InteroperabilityTestTopLevel extends js.Object {
  val value: js.String = ???
  def valueAsInt(): js.Number = ???
}

@JSName("InteroperabilityTestTopLevelObject")
object InteroperabilityTestTopLevel extends js.Object {
  def apply(value: js.String): InteroperabilityTestTopLevel = ???
}

object InteroperabilityTestGlobalScope extends js.GlobalScope {
  var interoperabilityTestGlobalScopeValue: js.String = ???
  def interoperabilityTestGlobalScopeValueAsInt(): js.Number = ???
}
