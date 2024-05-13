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

package org.scalajs.testsuite.compiler

import scala.scalajs.js
import scala.scalajs.js.annotation._

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.AssertThrows.assertThrows
import org.scalajs.testsuite.utils.JSAssert._
import org.scalajs.testsuite.utils.Platform._

/*
 * Based on examples in:
 * http://lampwww.epfl.ch/~doeraene/scala-js/doc/js-interoperability.html
 */
class InteroperabilityTest {
  import InteroperabilityTest._

  def assertArrayDynEquals(expected: js.Array[Any], actual: js.Dynamic): Unit = {
    assertJSArrayEquals(expected, actual.asInstanceOf[js.Array[Any]])
  }

  @Test def backquotesToEscapeScalaFields(): Unit = {
    val obj = js.eval("""
      var interoperabilityTestFieldEscape = {
        def: 0,
        val: function(x) { if (x) this.def = x; return this.def; }
      };
      interoperabilityTestFieldEscape;
    """).asInstanceOf[InteroperabilityTestFieldEscape]

    obj.`def` = 7357
    assertEquals(7357, obj.`def`)
    assertEquals(7357, obj.`val`())
    assertEquals(42, obj.`val`(42))
  }

  @Test def testJSNameToSpecifyTheJavaScriptNameForFields(): Unit = {
    val obj = js.eval("""
      var interoperabilityTestJSName = {
        def: 42,
        val: function(x) { if (x) this.def = x; return this.def; }
      };
      interoperabilityTestJSName;
    """).asInstanceOf[InteroperabilityTestJSName]

    assertEquals(42, obj.value())
    assertEquals(7357, obj.value(7357))
  }

  @Test def explicitGetterAndSetterNamesToFieldAccess(): Unit = {
    val obj = js.eval("""
      var interoperabilityTestProperty = { a: 1 };
      interoperabilityTestProperty;
      """).asInstanceOf[InteroperabilityTestProperty]

    assertEquals(1, obj.a)
    obj.a = 100
    assertEquals(100, obj.a)
  }

  @Test def testJSNameTogetherWithFieldAccess(): Unit = {
    val obj = js.eval("""
      var interoperabilityTestProperty = { b: 1 };
      interoperabilityTestProperty;
      """).asInstanceOf[InteroperabilityTestPropertyNamed]

    assertEquals(1, obj.a)
    obj.a = 100
    assertEquals(100, obj.a)
    assertEquals(100, obj.b)
  }

  @Test def testJSBracketAccessToSpecifyAccessUsingSquareBracketSubscription(): Unit = {
    val obj = js.eval("""
      var interoperabilityTestJSBracketAccess = [ 0, 1, 7357 ];
      interoperabilityTestJSBracketAccess;
    """).asInstanceOf[InteroperabilityTestJSBracketAccess]

    assertEquals(7357, obj(2))
    obj(2) = 42
    assertEquals(42, obj(2))
  }

  @Test def instantiationOfJSClassesInheritingFromJSObject(): Unit = {
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
    assertEquals(42, obj.field)
    assertEquals("42", obj.method())
    assertEquals("Scala.js", obj.getConstructorParam())
  }

  @Test def accessTopLevelJSObjectsViaScalaObjectsInheritingFromJSObject(): Unit = {
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
    assertEquals("7357", obj.value)
    assertEquals(7357, obj.valueAsInt())
  }

  @Test def accessNativeJSClassesAndObjectsNestedInJSObjects(): Unit = {
    js.eval("""
      var InteroperabilityTestContainerObject = {
        ContainedClass: function(x) {
          this.x = x;
        },
        ContainedObject: {
          x: 42
        },
        ContainedClassRenamed: function(x) {
          this.x = x * 2;
        },
        ContainedObjectRenamed: {
          x: 4242
        },
        ContainedClassWithDefaultParam: function(x) {
          this.x = x || 42;
        }
      };
    """)

    // Use alias for convenience: see end of file for definition
    val TopLevel = InteroperabilityTestContainerObject

    val obj1 = new TopLevel.ContainedClass(34)
    assertEquals(34, obj1.x)

    val obj2 = TopLevel.ContainedObject
    assertEquals(42, obj2.x)

    val obj3 = new TopLevel.ContainedClassWithJSName(65)
    assertEquals(130, obj3.x)

    val obj4 = TopLevel.ContainedObjectWithJSName
    assertEquals(4242, obj4.x)

    val obj5 = new TopLevel.ContainedClassWithDefaultParam()
    assertEquals(42, obj5.x)

    val obj6 = new TopLevel.ContainedClassWithDefaultParam(10)
    assertEquals(10, obj6.x)
  }

  @Test def nestedNativeJSClasses(): Unit = {
    js.eval("""
      var InteroperabilityTestContainerClass = (function(a) {
        this.ContainedClass = function(x) {
          this.x = x;
          this.foo = function(s) { return a + s + this.x; };
        };
        this.ContainedObject = {
          x: 42,
          foo: function(s) { return a + s + this.x; }
        };
        this.ContainedClassRenamed = function(x) {
          this.x = x * 2;
          this.foo = function(s) { return a + s + this.x; };
        };
        this.ContainedObjectRenamed = {
          x: 4242,
          foo: function(s) { return a + s + this.x; }
        };
        this.ContainedClassWithDefaultParam = function(x) {
          this.x = x || 42;
          this.foo = function(s) { return a + s + this.x; };
        };
      });
    """)

    val enclosing = new InteroperabilityTestContainerClass("abc ")

    val obj1 = new enclosing.ContainedClass(34)
    assertEquals(34, obj1.x)
    assertEquals("abc def 34", obj1.foo("def "))

    val obj2 = enclosing.ContainedObject
    assertEquals(42, obj2.x)
    assertEquals("abc def 42", obj2.foo("def "))

    val obj3 = new enclosing.ContainedClassWithJSName(65)
    assertEquals(130, obj3.x)
    assertEquals("abc def 130", obj3.foo("def "))

    val obj4 = enclosing.ContainedObjectWithJSName
    assertEquals(4242, obj4.x)
    assertEquals("abc def 4242", obj4.foo("def "))

    val obj5 = new enclosing.ContainedClassWithDefaultParam()
    assertEquals(42, obj5.x)
    assertEquals("abc def 42", obj5.foo("def "))

    val obj6 = new enclosing.ContainedClassWithDefaultParam(10)
    assertEquals(10, obj6.x)
    assertEquals("abc def 10", obj6.foo("def "))
  }

  @Test def accessNativeJSClassesAndObjectsNestedInAtJSNamedJSObjects(): Unit = {
    js.eval("""
      var InteroperabilityTestContainerObjectRenamed = {
        ContainedClass: function(x) {
          this.x = x;
        },
        ContainedObject: {
          x: 42
        },
        ContainedClassRenamed: function(x) {
          this.x = x * 2;
        },
        ContainedObjectRenamed: {
          x: 4242
        }
      };
    """)

    // Use alias for convenience: see end of file for definition
    val TopLevel = InteroperabilityTestContainerObjectExplicitName

    val obj1 = new TopLevel.ContainedClass(34)
    assertEquals(34, obj1.x)

    val obj2 = TopLevel.ContainedObject
    assertEquals(42, obj2.x)

    val obj3 = new TopLevel.ContainedClassWithJSName(65)
    assertEquals(130, obj3.x)

    val obj4 = TopLevel.ContainedObjectWithJSName
    assertEquals(4242, obj4.x)
  }

  @Test def callJSMethodsWithVariadicParameters(): Unit = {
    val obj = js.eval("""
      var obj = {
        foo: function() {
          var args = new Array(arguments.length);
          for (var i = 0; i < arguments.length; i++)
            args[i] = arguments[i];
          return args;
        }
      };
      obj;
    """)

    val elems = Seq[js.Any]("plop", 42, 51)

    val dyn = obj.asInstanceOf[js.Dynamic]
    assertArrayDynEquals(js.Array(), dyn.foo())
    assertArrayDynEquals(js.Array(3, 6), dyn.foo(3, 6))
    assertArrayDynEquals(js.Array("hello", false), dyn.foo("hello", false))
    assertArrayDynEquals(js.Array("plop", 42, 51), dyn.applyDynamic("foo")(elems: _*))

    val stat = obj.asInstanceOf[InteroperabilityTestVariadicMethod]
    assertJSArrayEquals(js.Array[Any](), stat.foo())
    assertJSArrayEquals(js.Array[Any](3, 6), stat.foo(3, 6))
    assertJSArrayEquals(js.Array("hello", false), stat.foo("hello", false))
    assertJSArrayEquals(js.Array("plop", 42, 51), stat.foo(elems: _*))
  }

  @Test def callPolytypeNullaryMethod_Issue2445(): Unit = {
    val obj = js.eval("""
      var obj = {
        emptyArray: []
      };
      obj;
    """)

    val objNative =
      obj.asInstanceOf[InteroperabilityTestPolyTypeNullaryMethodNative]
    val a = objNative.emptyArray[Int]
    assertTrue((a: Any).isInstanceOf[js.Array[_]])
    assertEquals(0, a.length)

    val objNonNative =
      obj.asInstanceOf[InteroperabilityTestPolyTypeNullaryMethodNonNative]
    val b = objNonNative.emptyArray[Int]
    assertTrue((b: Any).isInstanceOf[js.Array[_]])
    assertEquals(0, b.length)

    // From the bug report
    val objPoly =
      obj.asInstanceOf[InteroperabilityTestPolyClassPolyNullaryMethod[Int]]
    val c = objPoly.emptyArray[Any]
    assertTrue((c: Any).isInstanceOf[js.Array[_]])
    assertEquals(0, c.length)
  }

  @Test def callJSConstructorsWithVariadicParameters(): Unit = {
    import js.Dynamic.{newInstance => jsnew}

    js.eval("""
      var InteroperabilityTestVariadicCtor = function() {
        var args = new Array(arguments.length);
        for (var i = 0; i < arguments.length; i++)
          args[i] = arguments[i];
        this.args = args;
      };
    """)

    val elems = Seq[js.Any]("plop", 42, 51)

    val ctor = js.Dynamic.global.InteroperabilityTestVariadicCtor

    val args0 = jsnew(ctor)().args
    assertArrayDynEquals(js.Array(), args0)
    val args1 = jsnew(ctor)(3, 6).args
    assertArrayDynEquals(js.Array(3, 6), args1)
    val args2 = jsnew(ctor)("hello", false).args
    assertArrayDynEquals(js.Array("hello", false), args2)
    val args3 = jsnew(ctor)(elems: _*).args
    assertArrayDynEquals(js.Array("plop", 42, 51), args3)

    import org.scalajs.testsuite.compiler.{InteroperabilityTestVariadicCtor => C}
    assertJSArrayEquals(js.Array[Any](), new C().args)
    assertJSArrayEquals(js.Array[Any](3, 6), new C(3, 6).args)
    assertJSArrayEquals(js.Array("hello", false), new C("hello", false).args)
    assertJSArrayEquals(js.Array("plop", 42, 51), new C(elems: _*).args)
  }

  @Test def callJSConstructorsWithVarArgsWhenConstructIsRequired_Issue4362(): Unit = {
    assumeTrue("requires the spread operator", assumeES2015)

    @noinline def args(): Seq[Any] = Seq(1234)

    val dateObj = new InteroperabilityTestJSDateWithVarArgsConstructor(args(): _*)
    assertEquals(1234.0, dateObj.getTime(), 0.0)
  }

  @Test def accessTopLevelJSObjectsViaScalaObjectWithAnnotJSGlobalScope(): Unit = {
    js.eval("""
      var interoperabilityTestGlobalScopeValue = "7357";
      var interoperabilityTestGlobalScopeValueAsInt = function() {
        return parseInt(interoperabilityTestGlobalScopeValue);
      };
      var InteroperabilityTestGlobalScopeObject = {foo: 456};
      var InteroperabilityTestGlobalScopeClass = function() {
        this.bar = 654;
      };
    """)

    // Use alias for convenience: see end of file for definition
    import org.scalajs.testsuite.compiler.{InteroperabilityTestGlobalScope => Global}

    assertEquals("7357", Global.interoperabilityTestGlobalScopeValue)
    assertEquals(7357, Global.interoperabilityTestGlobalScopeValueAsInt())

    Global.interoperabilityTestGlobalScopeValue = "42"
    assertEquals(42, Global.interoperabilityTestGlobalScopeValueAsInt())

    assertEquals("object", js.typeOf(Global.InteroperabilityTestGlobalScopeObject))
    assertEquals(456, Global.InteroperabilityTestGlobalScopeObject.foo)

    assertEquals("function",
        js.typeOf(js.constructorOf[Global.InteroperabilityTestGlobalScopeClassRenamed]))
    val obj = new Global.InteroperabilityTestGlobalScopeClassRenamed
    assertEquals(654, obj.bar)
  }

  @Test def accessTopLevelJSVarsAndFunctionsViaScalaObjectWithNativeValsAndDefs(): Unit = {
    js.eval("""
      var interoperabilityTestGlobalValDefConstant = 654321;
      var interoperabilityTestGlobalValDefVariable = 7357;
      var interoperabilityTestGlobalValDefGetVariable = function() {
        return interoperabilityTestGlobalValDefVariable;
      }
      var interoperabilityTestGlobalValDefSetVariable = function(x) {
        interoperabilityTestGlobalValDefVariable = x;
      }
      var interoperabilityTestGlobalValDefFunction = function(x) {
        return interoperabilityTestGlobalValDefVariable + x;
      };
      var interoperabilityTestGlobalValDefFunctionWithDefaultParam = function(x, y) {
        return (x || 20) + (y || 5);
      };
    """)

    // Use alias for convenience: see end of file for definition
    import org.scalajs.testsuite.compiler.{InteroperabilityTestGlobalValsAndDefs => Global}

    assertEquals(654321, Global.interoperabilityTestGlobalValDefConstant)

    assertEquals(7357, Global.interoperabilityTestGlobalValDefVariable)
    assertEquals(7357, Global.interoperabilityTestGlobalValDefGetVariable())
    assertEquals(7360, Global.interoperabilityTestGlobalValDefFunction(3))

    Global.interoperabilityTestGlobalValDefSetVariable(123)
    assertEquals(123, Global.interoperabilityTestGlobalValDefGetVariable())
    assertEquals(126, Global.interoperabilityTestGlobalValDefFunction(3))
    Global.interoperabilityTestGlobalValDefSetVariable(7357)

    assertEquals(18, Global.interoperabilityTestGlobalValDefFunctionWithDefaultParam(10, 8))
    assertEquals(15, Global.interoperabilityTestGlobalValDefFunctionWithDefaultParam(10))
    assertEquals(25, Global.interoperabilityTestGlobalValDefFunctionWithDefaultParam())
    assertEquals(23, Global.interoperabilityTestGlobalValDefFunctionWithDefaultParam(y = 3))

    // Renamed

    assertEquals(654321, Global.interoperabilityTestGlobalValDefConstantRenamed)

    assertEquals(7357, Global.interoperabilityTestGlobalValDefVariableRenamed)
    assertEquals(7357, Global.interoperabilityTestGlobalValDefGetVariableRenamed())
    assertEquals(7360, Global.interoperabilityTestGlobalValDefFunctionRenamed(3))

    Global.interoperabilityTestGlobalValDefSetVariableRenamed(123)
    assertEquals(123, Global.interoperabilityTestGlobalValDefGetVariableRenamed())
    assertEquals(126, Global.interoperabilityTestGlobalValDefFunctionRenamed(3))
    Global.interoperabilityTestGlobalValDefSetVariableRenamed(7357)

    assertEquals(18, Global.interoperabilityTestGlobalValDefFunctionWithDefaultParamRenamed(10, 8))
    assertEquals(15, Global.interoperabilityTestGlobalValDefFunctionWithDefaultParamRenamed(10))
    assertEquals(25, Global.interoperabilityTestGlobalValDefFunctionWithDefaultParamRenamed())
    assertEquals(23, Global.interoperabilityTestGlobalValDefFunctionWithDefaultParamRenamed(y = 3))
  }

  @Test def accessTopLevelJSVarsAndFunctionsViaPackageObjectWithNativeValsAndDefs(): Unit = {
    js.eval("""
      var interoperabilityTestGlobalValDefConstantInPackageObject = 654321;
      var interoperabilityTestGlobalValDefVariableInPackageObject = 7357;
      var interoperabilityTestGlobalValDefGetVariableInPackageObject = function() {
        return interoperabilityTestGlobalValDefVariableInPackageObject;
      }
      var interoperabilityTestGlobalValDefSetVariableInPackageObject = function(x) {
        interoperabilityTestGlobalValDefVariableInPackageObject = x;
      }
      var interoperabilityTestGlobalValDefFunctionInPackageObject = function(x) {
        return interoperabilityTestGlobalValDefVariableInPackageObject + x;
      };
      var interoperabilityTestGlobalValDefFunctionWithDefaultParamInPackageObject = function(x, y) {
        return (x || 20) + (y || 5);
      };
    """)

    // Use alias for convenience: see end of file for definition
    import org.scalajs.testsuite.compiler.{interoperabilitytestglobalvalsanddefspackageobject => pack}

    assertEquals(654321, pack.interoperabilityTestGlobalValDefConstantInPackageObject)

    assertEquals(7357, pack.interoperabilityTestGlobalValDefVariableInPackageObject)
    assertEquals(7357, pack.interoperabilityTestGlobalValDefGetVariableInPackageObject())
    assertEquals(7360, pack.interoperabilityTestGlobalValDefFunctionInPackageObject(3))

    pack.interoperabilityTestGlobalValDefSetVariableInPackageObject(123)
    assertEquals(123, pack.interoperabilityTestGlobalValDefGetVariableInPackageObject())
    assertEquals(126, pack.interoperabilityTestGlobalValDefFunctionInPackageObject(3))
    pack.interoperabilityTestGlobalValDefSetVariableInPackageObject(7357)

    assertEquals(18, pack.interoperabilityTestGlobalValDefFunctionWithDefaultParamInPackageObject(10, 8))
    assertEquals(15, pack.interoperabilityTestGlobalValDefFunctionWithDefaultParamInPackageObject(10))
    assertEquals(25, pack.interoperabilityTestGlobalValDefFunctionWithDefaultParamInPackageObject())
    assertEquals(23, pack.interoperabilityTestGlobalValDefFunctionWithDefaultParamInPackageObject(y = 3))

    // Renamed

    assertEquals(654321, pack.interoperabilityTestGlobalValDefConstantInPackageObjectRenamed)

    assertEquals(7357, pack.interoperabilityTestGlobalValDefVariableInPackageObjectRenamed)
    assertEquals(7357, pack.interoperabilityTestGlobalValDefGetVariableInPackageObjectRenamed())
    assertEquals(7360, pack.interoperabilityTestGlobalValDefFunctionInPackageObjectRenamed(3))

    pack.interoperabilityTestGlobalValDefSetVariableInPackageObjectRenamed(123)
    assertEquals(123, pack.interoperabilityTestGlobalValDefGetVariableInPackageObjectRenamed())
    assertEquals(126, pack.interoperabilityTestGlobalValDefFunctionInPackageObjectRenamed(3))
    pack.interoperabilityTestGlobalValDefSetVariableInPackageObjectRenamed(7357)

    assertEquals(18, pack.interoperabilityTestGlobalValDefFunctionWithDefaultParamInPackageObjectRenamed(10, 8))
    assertEquals(15, pack.interoperabilityTestGlobalValDefFunctionWithDefaultParamInPackageObjectRenamed(10))
    assertEquals(25, pack.interoperabilityTestGlobalValDefFunctionWithDefaultParamInPackageObjectRenamed())
    assertEquals(23, pack.interoperabilityTestGlobalValDefFunctionWithDefaultParamInPackageObjectRenamed(y = 3))
  }


  @Test def protectReceiverOfJSApplyIfItsSelect_Issue804(): Unit = {
    val obj = js.eval("""
      var interoperabilityTestJSFunctionFieldApply = {
        toString: function() { return "bad" },
        check: function(x) { "use strict"; return this ? this.toString() : x; }
      };
      interoperabilityTestJSFunctionFieldApply;
    """).asInstanceOf[InteroperabilityTestJSFunctionFieldApply]

    assertEquals(7357, obj.check(7357))

    val check = obj.check
    assertEquals(0x600d, check(0x600d))

    class InScalaSelect(check: js.Function1[Int, Any]) {
      override def toString(): String = "bad"
      def test(): Unit = assertEquals(5894, check(5894))
    }
    new InScalaSelect(check).test()
  }

  @Test def handleDefaultParameters(): Unit = {
    val obj = js.eval("""
      var interoperabilityTestDefaultParam = {
        fun: function() { return Array.prototype.slice.call(arguments); }
      };
      interoperabilityTestDefaultParam;
    """).asInstanceOf[InteroperabilityTestDefaultParam]

    assertJSArrayEquals[Any](js.Array(1), obj.simple(1))
    assertJSArrayEquals[Any](js.Array(1, 5), obj.simple(1, 5))

    assertJSArrayEquals[Any](js.Array(js.undefined, 5), obj.named(y = 5))
    assertJSArrayEquals[Any](js.Array(5), obj.named(x = 5))

    assertJSArrayEquals[Any](js.Array(js.undefined, 1, 2, 3, 4), obj.multi()(1, 2, 3, 4)())
    assertJSArrayEquals[Any](js.Array(2, 5), obj.multi(2)()(5))

    // #4684 Default params with Unit type
    assertJSArrayEquals[Any](js.Array(()), obj.unitParam(()))
    assertJSArrayEquals[Any](js.Array((), ()), obj.unitParam((), ()))
  }

  @Test def defaultParametersForConstructors_Issue791(): Unit = {
    js.eval("""
      var InteroperabilityTestCtor = function(x,y) {
        this.values = Array(x || 6, y || 8)
      }
    """);

    import InteroperabilityTestScalaObjectContainer._

    assertJSArrayEquals(js.Array(6, 8), new InteroperabilityTestCtor().values)
    assertJSArrayEquals(js.Array(6, 7), new InteroperabilityTestCtor(y = 7).values)
    assertJSArrayEquals(js.Array(3, 8), new InteroperabilityTestCtor(3).values)
    assertJSArrayEquals(js.Array(10, 2), new InteroperabilityTestCtor(10, 2).values)
  }

  @Test def constructorParamsThatAreValsVarsInFacades_Issue1277(): Unit = {
    js.eval("""
        var InteroparabilityCtorInlineValue = function(x,y) {
          this.x = x;
          this.y = y;
        }
    """)

    import InteroperabilityTestScalaObjectContainer._

    val obj = new InteroparabilityCtorInlineValue(10, -1)

    assertEquals(10, obj.x)
    assertEquals(-1, obj.y)

    obj.y = 100

    assertEquals(10, obj.x)
    assertEquals(100, obj.y)
  }

  @Test def unboxCharsReceivedFromCallingJSInteropMethod(): Unit = {
    val obj = js.eval("""
      var obj = {
        anyAsChar: function(x) { return x; }
      };
      obj;
    """).asInstanceOf[InteroperabilityTestCharResult]

    @noinline def eCharAsAny: Any = Character.valueOf('e')
    val c: Char = obj.anyAsChar(eCharAsAny)

    /* Do not use `assertEquals` otherwise it would re-box the Char, defeating
     * the purpose of this test.
     */
    assertTrue('e' == c)
  }

  @Test def boxCharsGivenToJSInteropMethod(): Unit = {
    val obj = js.eval("""
      var obj = {
        charAsAny: function(c) { return c; }
      };
      obj;
    """).asInstanceOf[InteroperabilityTestCharParam]

    val any: Any = obj.charAsAny('x')
    assertTrue(any.isInstanceOf[Character])
    assertEquals('x', any)
  }

  @Test def unboxValueClassReceivedFromCallingJSInteropMethod(): Unit = {
    val obj = js.eval("""
      var obj = {
        test: function(vc) { return vc; }
      };
      obj;
    """).asInstanceOf[InteroperabilityTestValueClassResult]

    val r = obj.test(new SomeValueClass(5))
    assertEquals(5, r.i)
  }

  @Test def boxValueClassesGivenToJSInteropMethod(): Unit = {
    val obj = js.eval("""
      var obj = {
        test: function(vc) { return vc; }
      };
      obj;
    """).asInstanceOf[InteroperabilityTestValueClassParam]

    val vc = new SomeValueClass(7)
    val r = obj.test(vc)
    assertTrue(r.isInstanceOf[SomeValueClass])
    assertEquals(7, r.asInstanceOf[SomeValueClass].i)
  }

  @Test def doNotUnboxValuesReceivedFromJSMethodInStatementPosition(): Unit = {
    /* To test this, we verify that a purposefully ill-typed facade does not
     * throw a ClassCastException when called in statement position.
     */
    val obj = js.eval("""
      var obj = {
        test: function() { return 4; } // typed as String in the trait
      };
      obj;
    """).asInstanceOf[InteroperabilityTestNoUnboxResultInStatement]
    obj.test() // in statement position, should not throw
    if (hasCompliantAsInstanceOfs)
      assertThrows(classOf[Exception], obj.test()) // in expression position, should throw
  }

  @Test def asInstanceOfValuesReceivedFromCallingJSInteropMethod(): Unit = {
    assumeTrue("Assumed compliant asInstanceOf", hasCompliantAsInstanceOfs)
    val obj = js.eval("""
      var obj = {
        testChar: function() { return 5; },
        testInt: function() { return 6.4; },
        testShort: function() { return 60000; },
        testDouble: function() { return "hello"; },
        testString: function() { return {}; },
        testValueClass: function() { return "hello"; },
        testNormalClass: function() { return 45; },
        testAny: function() { return {}; }
      };
      obj;
    """).asInstanceOf[InteroperabilityTestAsInstanceOfResult]

    assertThrows(classOf[Exception], obj.testChar())
    assertThrows(classOf[Exception], obj.testInt())
    assertThrows(classOf[Exception], obj.testShort())
    assertThrows(classOf[Exception], obj.testDouble())
    assertThrows(classOf[Exception], obj.testString())
    assertThrows(classOf[Exception], obj.testValueClass())
    assertThrows(classOf[Exception], obj.testNormalClass())
    obj.testAny() // should not throw
  }

  @Test def accessGlobalScopeForObject(): Unit = {
    assumeTrue("Assuming execution in Node.js", executingInNodeJS)

    nodejs_runInThisContext("""
      const InteroperabilityTestConstObject = {
        x: 42
      };
    """)

    import InteroperabilityTestScalaObjectContainer._

    assertEquals("object", js.typeOf(InteroperabilityTestConstObject))
    assertEquals(42, InteroperabilityTestConstObject.x)
  }

  @Test def accessGlobalScopeForClass(): Unit = {
    assumeTrue("Assuming execution in Node.js", executingInNodeJS)

    nodejs_runInThisContext("""
      class InteroperabilityTestConstClass {
        constructor(x) {
          this.x = x;
        }
      };
    """)

    import InteroperabilityTestScalaObjectContainer._

    assertEquals("function",
        js.typeOf(js.constructorOf[InteroperabilityTestConstClass]))
    val obj = new InteroperabilityTestConstClass(5)
    assertEquals(5, obj.x)
  }

  @Test def accessGlobalScopeForJSGlobalScopeMembers(): Unit = {
    assumeTrue("Assuming execution in Node.js", executingInNodeJS)

    nodejs_runInThisContext("""
      const InteroperabilityTestLetConstGlobals_value = 456;
      let InteroperabilityTestLetConstGlobals_variable = "hello";
      const InteroperabilityTestLetConstGlobals_method = (function(x) {
        return x + 1;
      });
    """)

    import InteroperabilityTestScalaObjectContainer._
    import InteroperabilityTestLetConstGlobals._

    assertEquals("number", js.typeOf(InteroperabilityTestLetConstGlobals_value))
    assertEquals(456, InteroperabilityTestLetConstGlobals_value)

    assertEquals("string", js.typeOf(InteroperabilityTestLetConstGlobals_variable))
    assertEquals("hello", InteroperabilityTestLetConstGlobals_variable)
    InteroperabilityTestLetConstGlobals_variable = "world"
    assertEquals("world", InteroperabilityTestLetConstGlobals_variable)

    assertEquals(6, InteroperabilityTestLetConstGlobals_method(5))
  }

}

object InteroperabilityTest {

  @js.native
  trait InteroperabilityTestFieldEscape extends js.Object {
    var `def`: Int = js.native
    def `val`(): Int = js.native
    def `val`(n: Int): Int = js.native
  }

  @js.native
  trait InteroperabilityTestJSName extends js.Object {
    @JSName("val")
    def value(): Int = js.native
    @JSName("val")
    def value(n: Int): Int = js.native
  }

  @js.native
  trait InteroperabilityTestProperty extends js.Object {
    def a_=(x: Int): Unit = js.native
    def a: Int = js.native
  }

  @js.native
  trait InteroperabilityTestPropertyNamed extends js.Object {
    @JSName("b")
    def a_=(x: Int): Unit = js.native
    @JSName("b")
    def a: Int = js.native
    def b: Int = js.native
  }

  @js.native
  trait InteroperabilityTestJSBracketAccess extends js.Object {
    @JSBracketAccess
    def apply(index: Int): Int = js.native
    @JSBracketAccess
    def update(index: Int, v: Int): Unit = js.native
  }

  @js.native
  trait InteroperabilityTestJSFunctionFieldApply extends js.Object {
    val check: js.Function1[Int, Any] = js.native
  }

  /** Trait with different method signatures, all forwarded to the same JS
   *  function that returns the argument list for inspection.
   */
  @js.native
  trait InteroperabilityTestDefaultParam extends js.Object {
    @JSName("fun")
    def simple(x: Int, y: Int = 5): js.Array[Any] = js.native
    @JSName("fun")
    def named(x: Int = 1, y: Int = 1, z: Int = 1): js.Array[Any] = js.native
    @JSName("fun")
    def multi(x: Int = 1)(ys: Int*)(z: Int = 1): js.Array[Any] = js.native
    @JSName("fun")
    def unitParam(x: Unit, y: Unit = ()): js.Array[Any] = js.native
  }

  @js.native
  trait InteroperabilityTestCharResult extends js.Object {
    def anyAsChar(x: Any): Char = js.native
  }

  @js.native
  trait InteroperabilityTestCharParam extends js.Object {
    def charAsAny(c: Char): Any = js.native
  }

  @js.native
  trait InteroperabilityTestValueClassResult extends js.Object {
    def test(vc: Any): SomeValueClass = js.native
  }

  @js.native
  trait InteroperabilityTestValueClassParam extends js.Object {
    def test(vc: SomeValueClass): Any = js.native
  }

  @js.native
  trait InteroperabilityTestNoUnboxResultInStatement extends js.Object {
    def test(): String = js.native
  }

  @js.native
  trait InteroperabilityTestAsInstanceOfResult extends js.Object {
    def testChar(): Char = js.native
    def testInt(): Int = js.native
    def testShort(): Short = js.native
    def testDouble(): Double = js.native
    def testString(): String = js.native
    def testValueClass(): SomeValueClass = js.native
    def testNormalClass(): List[Int] = js.native
    def testAny(): Any = js.native
  }
}

/*
 * Helper classes, traits and objects defined here since they cannot be nested
 * without requiring explicit @JSName's, which would defeat the purpose of
 * their tests.
 */

@JSGlobal("InteroperabilityTestInherit.Pattern")
@js.native
class InteroperabilityTestPattern protected () extends js.Object {
  def this(pattern: String) = this()
  val field: Int = js.native
  def method(): String = js.native
  def getConstructorParam(): String = js.native
}

@js.native
trait InteroperabilityTestTopLevel extends js.Object {
  val value: String = js.native
  def valueAsInt(): Int = js.native
}

@JSGlobal("InteroperabilityTestTopLevelObject")
@js.native
object InteroperabilityTestTopLevel extends js.Object {
  def apply(value: String): InteroperabilityTestTopLevel = js.native
}

@js.native
@JSGlobal
object InteroperabilityTestContainerObject extends js.Object {
  @js.native
  class ContainedClass(_x: Int) extends js.Object {
    val x: Int = js.native
  }

  @js.native
  object ContainedObject extends js.Object {
    val x: Int = js.native
  }

  @JSName("ContainedClassRenamed")
  @js.native
  class ContainedClassWithJSName(_x: Int) extends js.Object {
    val x: Int = js.native
  }

  @JSName("ContainedObjectRenamed")
  @js.native
  object ContainedObjectWithJSName extends js.Object {
    val x: Int = js.native
  }

  @js.native
  class ContainedClassWithDefaultParam(_x: Int = ???) extends js.Object {
    val x: Int = js.native
  }
}

@js.native
@JSGlobal
class InteroperabilityTestContainerClass(a: String) extends js.Object {
  @js.native
  class ContainedClass(_x: Int) extends js.Object {
    val x: Int = js.native
    def foo(s: String): String = js.native
  }

  @js.native
  object ContainedObject extends js.Object {
    val x: Int = js.native
    def foo(s: String): String = js.native
  }

  @JSName("ContainedClassRenamed")
  @js.native
  class ContainedClassWithJSName(_x: Int) extends js.Object {
    val x: Int = js.native
    def foo(s: String): String = js.native
  }

  @JSName("ContainedObjectRenamed")
  @js.native
  object ContainedObjectWithJSName extends js.Object {
    val x: Int = js.native
    def foo(s: String): String = js.native
  }

  @js.native
  class ContainedClassWithDefaultParam(_x: Int = ???) extends js.Object {
    val x: Int = js.native
    def foo(s: String): String = js.native
  }
}

@JSGlobal("InteroperabilityTestContainerObjectRenamed")
@js.native
object InteroperabilityTestContainerObjectExplicitName extends js.Object {
  @js.native
  class ContainedClass(_x: Int) extends js.Object {
    val x: Int = js.native
  }

  @js.native
  object ContainedObject extends js.Object {
    val x: Int = js.native
  }

  @JSName("ContainedClassRenamed")
  @js.native
  class ContainedClassWithJSName(_x: Int) extends js.Object {
    val x: Int = js.native
  }

  @JSName("ContainedObjectRenamed")
  @js.native
  object ContainedObjectWithJSName extends js.Object {
    val x: Int = js.native
  }
}

@js.native
trait InteroperabilityTestVariadicMethod extends js.Object {
  def foo(args: Any*): js.Array[Any] = js.native
}

@js.native
trait InteroperabilityTestPolyTypeNullaryMethodNative extends js.Object {
  def emptyArray[T]: js.Array[T] = js.native
}

trait InteroperabilityTestPolyTypeNullaryMethodNonNative extends js.Object {
  def emptyArray[T]: js.Array[T]
}

@js.native
trait InteroperabilityTestPolyClassPolyNullaryMethod[+T] extends js.Object {
  def emptyArray[T2 >: T]: js.Array[T2] = js.native
}

@js.native
@JSGlobal
class InteroperabilityTestVariadicCtor(inargs: Any*) extends js.Object {
  val args: js.Array[Any] = js.native
}

@js.native
@JSGlobal("Date")
class InteroperabilityTestJSDateWithVarArgsConstructor(args: Any*) extends js.Object {
  def getTime(): Double = js.native
}

@js.native
@JSGlobalScope
object InteroperabilityTestGlobalScope extends js.Object {
  var interoperabilityTestGlobalScopeValue: String = js.native
  def interoperabilityTestGlobalScopeValueAsInt(): Int = js.native

  @js.native
  object InteroperabilityTestGlobalScopeObject extends js.Object {
    val foo: Int = js.native
  }

  @js.native
  @JSName("InteroperabilityTestGlobalScopeClass")
  class InteroperabilityTestGlobalScopeClassRenamed extends js.Object {
    val bar: Int = js.native
  }
}

object InteroperabilityTestGlobalValsAndDefs {
  @js.native
  @JSGlobal
  val interoperabilityTestGlobalValDefConstant: Int = js.native

  @js.native
  @JSGlobal("interoperabilityTestGlobalValDefConstant")
  val interoperabilityTestGlobalValDefConstantRenamed: Int = js.native

  @js.native
  @JSGlobal
  def interoperabilityTestGlobalValDefVariable: Int = js.native

  @js.native
  @JSGlobal("interoperabilityTestGlobalValDefVariable")
  def interoperabilityTestGlobalValDefVariableRenamed: Int = js.native

  @js.native
  @JSGlobal
  def interoperabilityTestGlobalValDefGetVariable(): Int = js.native

  @js.native
  @JSGlobal("interoperabilityTestGlobalValDefGetVariable")
  def interoperabilityTestGlobalValDefGetVariableRenamed(): Int = js.native

  @js.native
  @JSGlobal
  def interoperabilityTestGlobalValDefSetVariable(x: Int): Unit = js.native

  @js.native
  @JSGlobal("interoperabilityTestGlobalValDefSetVariable")
  def interoperabilityTestGlobalValDefSetVariableRenamed(x: Int): Unit = js.native

  @js.native
  @JSGlobal
  def interoperabilityTestGlobalValDefFunction(x: Int): Int = js.native

  @js.native
  @JSGlobal("interoperabilityTestGlobalValDefFunction")
  def interoperabilityTestGlobalValDefFunctionRenamed(x: Int): Int = js.native

  /* In this facade, 50 is not the actual default value for `y`.
   * We intentionally use a different value to check that it is ignored.
   * See #4554.
   * The default value `= js.native` of `x` is a test for #4553.
   */
  @js.native
  @JSGlobal
  def interoperabilityTestGlobalValDefFunctionWithDefaultParam(x: Int = js.native, y: Int = 50): Int = js.native

  @js.native
  @JSGlobal("interoperabilityTestGlobalValDefFunctionWithDefaultParam")
  def interoperabilityTestGlobalValDefFunctionWithDefaultParamRenamed(x: Int = js.native, y: Int = 50): Int = js.native
}

package object interoperabilitytestglobalvalsanddefspackageobject {
  @js.native
  @JSGlobal
  val interoperabilityTestGlobalValDefConstantInPackageObject: Int = js.native

  @js.native
  @JSGlobal("interoperabilityTestGlobalValDefConstantInPackageObject")
  val interoperabilityTestGlobalValDefConstantInPackageObjectRenamed: Int = js.native

  @js.native
  @JSGlobal
  def interoperabilityTestGlobalValDefVariableInPackageObject: Int = js.native

  @js.native
  @JSGlobal("interoperabilityTestGlobalValDefVariableInPackageObject")
  def interoperabilityTestGlobalValDefVariableInPackageObjectRenamed: Int = js.native

  @js.native
  @JSGlobal
  def interoperabilityTestGlobalValDefGetVariableInPackageObject(): Int = js.native

  @js.native
  @JSGlobal("interoperabilityTestGlobalValDefGetVariableInPackageObject")
  def interoperabilityTestGlobalValDefGetVariableInPackageObjectRenamed(): Int = js.native

  @js.native
  @JSGlobal
  def interoperabilityTestGlobalValDefSetVariableInPackageObject(x: Int): Unit = js.native

  @js.native
  @JSGlobal("interoperabilityTestGlobalValDefSetVariableInPackageObject")
  def interoperabilityTestGlobalValDefSetVariableInPackageObjectRenamed(x: Int): Unit = js.native

  @js.native
  @JSGlobal
  def interoperabilityTestGlobalValDefFunctionInPackageObject(x: Int): Int = js.native

  @js.native
  @JSGlobal("interoperabilityTestGlobalValDefFunctionInPackageObject")
  def interoperabilityTestGlobalValDefFunctionInPackageObjectRenamed(x: Int): Int = js.native

  /* In this facade, 50 is not the actual default value for `y`.
   * We intentionally use a different value to check that it is ignored.
   * See #4554.
   * The default value `= js.native` of `x` is a test for #4553.
   */
  @js.native
  @JSGlobal("interoperabilityTestGlobalValDefFunctionWithDefaultParamInPackageObject")
  def interoperabilityTestGlobalValDefFunctionWithDefaultParamInPackageObject(
      x: Int = js.native, y: Int = 50): Int = js.native

  @js.native
  @JSGlobal("interoperabilityTestGlobalValDefFunctionWithDefaultParamInPackageObject")
  def interoperabilityTestGlobalValDefFunctionWithDefaultParamInPackageObjectRenamed(
      x: Int = js.native, y: Int = 50): Int = js.native
}

class SomeValueClass(val i: Int) extends AnyVal {
  override def toString(): String = s"SomeValueClass($i)"
}

object InteroperabilityTestScalaObjectContainer {
  @js.native
  @JSGlobal
  class InteroperabilityTestCtor(x: Int = 5, y: Int = ???) extends js.Object {
    def values: js.Array[Int] = js.native
  }

  @js.native
  @JSGlobal
  class InteroparabilityCtorInlineValue(val x: Int, var y: Int) extends js.Object

  @js.native
  @JSGlobal
  object InteroperabilityTestConstObject extends js.Object {
    val x: Int = js.native
  }

  @js.native
  @JSGlobal
  class InteroperabilityTestConstClass(val x: Int) extends js.Object

  @js.native
  @JSGlobalScope
  object InteroperabilityTestLetConstGlobals extends js.Any {
    val InteroperabilityTestLetConstGlobals_value: Int = js.native
    var InteroperabilityTestLetConstGlobals_variable: String = js.native
    def InteroperabilityTestLetConstGlobals_method(x: Int): Int = js.native
  }
}
