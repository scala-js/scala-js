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
import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global

import org.scalajs.testsuite.utils.AssertThrows.assertThrows
import org.scalajs.testsuite.utils.JSAssert._
import org.scalajs.testsuite.utils.JSUtils
import org.scalajs.testsuite.utils.Platform._

import scala.annotation.meta

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.{global => globalEc}

import org.junit.Assert._
import org.junit.Assume._
import org.junit.Test

import org.scalajs.junit.async._

class TopLevelExportsTest {

  /** The namespace in which top-level exports are stored. */
  private lazy val exportsNamespace: Future[js.Dynamic] =
    ExportLoopback.exportsNamespace

  def witnessOf(obj: Any): Any = {
    assertTrue("" + obj.getClass(), obj.isInstanceOf[WitnessInterface])
    obj.asInstanceOf[WitnessInterface].witness
  }

  // @JSExportTopLevel classes and objects

  @Test def toplevelExportsForObjects(): AsyncResult = await {
    val objFuture =
      if (isNoModule) Future.successful(global.TopLevelExportedObject)
      else exportsNamespace.map(_.TopLevelExportedObject)
    for (obj <- objFuture) yield {
      assertJSNotUndefined(obj)
      assertEquals("object", js.typeOf(obj))
      assertEquals("witness", witnessOf(obj))
    }
  }

  @Test def toplevelExportsForScalaJSDefinedJSObjects(): AsyncResult = await {
    val obj1Future =
      if (isNoModule) Future.successful(global.SJSDefinedTopLevelExportedObject)
      else exportsNamespace.map(_.SJSDefinedTopLevelExportedObject)
    for (obj1 <- obj1Future) yield {
      assertJSNotUndefined(obj1)
      assertEquals("object", js.typeOf(obj1))
      assertEquals("witness", obj1.witness)

      assertSame(obj1, SJSDefinedExportedObject)
    }
  }

  @Test def toplevelExportsForNestedObjects(): AsyncResult = await {
    val objFuture =
      if (isNoModule) Future.successful(global.NestedExportedObject)
      else exportsNamespace.map(_.NestedExportedObject)
    for (obj <- objFuture) yield {
      assertJSNotUndefined(obj)
      assertEquals("object", js.typeOf(obj))
      assertSame(obj, ExportHolder.ExportedObject)
    }
  }

  @Test def exportsForObjectsWithConstantFoldedName(): AsyncResult = await {
    val objFuture =
      if (isNoModule) Future.successful(global.ConstantFoldedObjectExport)
      else exportsNamespace.map(_.ConstantFoldedObjectExport)
    for (obj <- objFuture) yield {
      assertJSNotUndefined(obj)
      assertEquals("object", js.typeOf(obj))
      assertEquals("witness", witnessOf(obj))
    }
  }

  @Test def exportsForProtectedObjects(): AsyncResult = await {
    val objFuture =
      if (isNoModule) Future.successful(global.ProtectedExportedObject)
      else exportsNamespace.map(_.ProtectedExportedObject)
    for (obj <- objFuture) yield {
      assertJSNotUndefined(obj)
      assertEquals("object", js.typeOf(obj))
      assertEquals("witness", witnessOf(obj))
    }
  }

  @Test def toplevelExportsForClasses(): AsyncResult = await {
    val constrFuture =
      if (isNoModule) Future.successful(global.TopLevelExportedClass)
      else exportsNamespace.map(_.TopLevelExportedClass)
    for (constr <- constrFuture) yield {
      assertJSNotUndefined(constr)
      assertEquals("function", js.typeOf(constr))
      val obj = js.Dynamic.newInstance(constr)(5)
      assertEquals(5, witnessOf(obj))
    }
  }

  @Test def toplevelExportsForScalaJSDefinedJSClasses(): AsyncResult = await {
    val constrFuture =
      if (isNoModule) Future.successful(global.SJSDefinedTopLevelExportedClass)
      else exportsNamespace.map(_.SJSDefinedTopLevelExportedClass)
    for (constr <- constrFuture) yield {
      assertJSNotUndefined(constr)
      assertEquals("function", js.typeOf(constr))
      val obj = js.Dynamic.newInstance(constr)(5)
      assertTrue((obj: Any).isInstanceOf[SJSDefinedTopLevelExportedClass])
      assertEquals(5, obj.x)

      assertSame(constr, js.constructorOf[SJSDefinedTopLevelExportedClass])
    }
  }

  @Test def toplevelExportsForAbstractJSClasses_Issue4117(): AsyncResult = await {
    val constrFuture =
      if (isNoModule) Future.successful(global.TopLevelExportedAbstractJSClass)
      else exportsNamespace.map(_.TopLevelExportedAbstractJSClass)

    for (constr <- constrFuture) yield {
      assertEquals("function", js.typeOf(constr))

      val body = {
        """
        class SubClass extends constr {
          constructor(x) {
            super(x);
          }
          foo(y) {
             return y + this.x;
          }
        }
        return SubClass;
        """
      }

      val subclassFun = new js.Function("constr", body)
        .asInstanceOf[js.Function1[js.Dynamic, js.Dynamic]]
      val subclass = subclassFun(constr)
      assertEquals("function", js.typeOf(subclass))

      val obj = js.Dynamic.newInstance(subclass)(5)
        .asInstanceOf[TopLevelExportedAbstractJSClass]

      assertEquals(5, obj.x)
      assertEquals(11, obj.foo(6))
      assertEquals(33, obj.bar(6))
    }
  }

  @Test def toplevelExportsForNestedClasses(): AsyncResult = await {
    val constrFuture =
      if (isNoModule) Future.successful(global.NestedExportedClass)
      else exportsNamespace.map(_.NestedExportedClass)
    for (constr <- constrFuture) yield {
      assertJSNotUndefined(constr)
      assertEquals("function", js.typeOf(constr))
      val obj = js.Dynamic.newInstance(constr)()
      assertTrue((obj: Any).isInstanceOf[ExportHolder.ExportedClass])
    }
  }

  @Test def toplevelExportsForNestedSjsDefinedClasses(): AsyncResult = await {
    val constrFuture =
      if (isNoModule) Future.successful(global.NestedSJSDefinedExportedClass)
      else exportsNamespace.map(_.NestedSJSDefinedExportedClass)
    for (constr <- constrFuture) yield {
      assertJSNotUndefined(constr)
      assertEquals("function", js.typeOf(constr))
      val obj = js.Dynamic.newInstance(constr)()
      assertTrue((obj: Any).isInstanceOf[ExportHolder.SJSDefinedExportedClass])
    }
  }

  @Test def exportsForClassesWithConstantFoldedName(): AsyncResult = await {
    val constrFuture =
      if (isNoModule) Future.successful(global.ConstantFoldedClassExport)
      else exportsNamespace.map(_.ConstantFoldedClassExport)
    for (constr <- constrFuture) yield {
      assertJSNotUndefined(constr)
      assertEquals("function", js.typeOf(constr))
      val obj = js.Dynamic.newInstance(constr)(5)
      assertEquals(5, witnessOf(obj))
    }
  }

  @Test def exportsForProtectedClasses(): AsyncResult = await {
    val constrFuture =
      if (isNoModule) Future.successful(global.ProtectedExportedClass)
      else exportsNamespace.map(_.ProtectedExportedClass)
    for (constr <- constrFuture) yield {
      assertJSNotUndefined(constr)
      assertEquals("function", js.typeOf(constr))
      val obj = js.Dynamic.newInstance(constr)(5)
      assertEquals(5, witnessOf(obj))
    }
  }

  @Test def exportForClassesWithRepeatedParametersInCtor(): AsyncResult = await {
    val constrFuture =
      if (isNoModule) Future.successful(global.ExportedVarArgClass)
      else exportsNamespace.map(_.ExportedVarArgClass)
    for (constr <- constrFuture) yield {
      assertEquals("", witnessOf(js.Dynamic.newInstance(constr)()))
      assertEquals("a", witnessOf(js.Dynamic.newInstance(constr)("a")))
      assertEquals("a|b", witnessOf(js.Dynamic.newInstance(constr)("a", "b")))
      assertEquals("a|b|c", witnessOf(js.Dynamic.newInstance(constr)("a", "b", "c")))
      assertEquals("Number: <5>|a", witnessOf(js.Dynamic.newInstance(constr)(5, "a")))
    }
  }

  @Test def exportForClassesWithDefaultParametersInCtor(): AsyncResult = await {
    val constrFuture =
      if (isNoModule) Future.successful(global.ExportedDefaultArgClass)
      else exportsNamespace.map(_.ExportedDefaultArgClass)
    for (constr <- constrFuture) yield {
      assertEquals(6, witnessOf(js.Dynamic.newInstance(constr)(1, 2, 3)))
      assertEquals(106, witnessOf(js.Dynamic.newInstance(constr)(1)))
      assertEquals(103, witnessOf(js.Dynamic.newInstance(constr)(1, 2)))
    }
  }

  // @JSExportTopLevel methods

  @Test def basicTopLevelExport(): Unit = {
    assumeTrue("Assume NoModule", isNoModule)
    assertEquals(1, global.TopLevelExport_basic())
  }

  @Test def basicTopLevelExportModule(): AsyncResult = await {
    assumeFalse("Assume Module", isNoModule)
    for (exp <- exportsNamespace) yield {
      assertEquals(1, exp.TopLevelExport_basic())
    }
  }

  @Test def overloadedTopLevelExport(): Unit = {
    assumeTrue("Assume NoModule", isNoModule)
    assertEquals("Hello World", global.TopLevelExport_overload("World"))
    assertEquals(2, global.TopLevelExport_overload(2))
    assertEquals(9, global.TopLevelExport_overload(2, 7))
    assertEquals(10, global.TopLevelExport_overload(1, 2, 3, 4))
  }

  @Test def overloadedTopLevelExportModule(): AsyncResult = await {
    assumeFalse("Assume Module", isNoModule)
    for (exp <- exportsNamespace) yield {
      assertEquals("Hello World", exp.TopLevelExport_overload("World"))
      assertEquals(2, exp.TopLevelExport_overload(2))
      assertEquals(9, exp.TopLevelExport_overload(2, 7))
      assertEquals(10, exp.TopLevelExport_overload(1, 2, 3, 4))
    }
  }

  @Test def defaultParamsTopLevelExport_Issue4052(): Unit = {
    assumeTrue("Assume NoModule", isNoModule)
    assertEquals(7, global.TopLevelExport_defaultParams(6))
    assertEquals(11, global.TopLevelExport_defaultParams(6, 5))
  }

  @Test def defaultParamsTopLevelExportModule_Issue4052(): AsyncResult = await {
    assumeFalse("Assume Module", isNoModule)
    for (exp <- exportsNamespace) yield {
      assertEquals(7, exp.TopLevelExport_defaultParams(6))
      assertEquals(11, exp.TopLevelExport_defaultParams(6, 5))
    }
  }

  @Test def topLevelExportUsesUniqueObject(): Unit = {
    assumeTrue("Assume NoModule", isNoModule)
    global.TopLevelExport_set(3)
    assertEquals(3, TopLevelExports.myVar)
    global.TopLevelExport_set(7)
    assertEquals(7, TopLevelExports.myVar)
  }

  @Test def topLevelExportUsesUniqueObjectModule(): AsyncResult = await {
    assumeFalse("Assume Module", isNoModule)
    for (exp <- exportsNamespace) yield {
      exp.TopLevelExport_set(3)
      assertEquals(3, TopLevelExports.myVar)
      exp.TopLevelExport_set(7)
      assertEquals(7, TopLevelExports.myVar)
    }
  }

  @Test def topLevelExportFromNestedObject(): Unit = {
    assumeTrue("Assume NoModule", isNoModule)
    global.TopLevelExport_setNested(28)
    assertEquals(28, TopLevelExports.Nested.myVar)
  }

  @Test def topLevelExportFromNestedObjectModule(): AsyncResult = await {
    assumeFalse("Assume Module", isNoModule)
    for (exp <- exportsNamespace) yield {
      exp.TopLevelExport_setNested(28)
      assertEquals(28, TopLevelExports.Nested.myVar)
    }
  }

  @Test def topLevelExportWithDoubleUnderscore(): Unit = {
    assumeTrue("Assume NoModule", isNoModule)
    assertEquals(true, global.__topLevelExportWithDoubleUnderscore)
  }

  @Test def topLevelExportWithDoubleUnderscoreModule(): AsyncResult = await {
    assumeFalse("Assume Module", isNoModule)
    for (exp <- exportsNamespace) yield {
      assertEquals(true, exp.__topLevelExportWithDoubleUnderscore)
    }
  }

  @Test def topLevelExportIsAlwaysReachable(): Unit = {
    assumeTrue("Assume NoModule", isNoModule)
    assertEquals("Hello World", global.TopLevelExport_reachability())
  }

  @Test def topLevelExportIsAlwaysReachableModule(): AsyncResult = await {
    assumeFalse("Assume Module", isNoModule)
    for (exp <- exportsNamespace) yield {
      assertEquals("Hello World", exp.TopLevelExport_reachability())
    }
  }

  // @JSExportTopLevel fields

  @Test def topLevelExportBasicField(): Unit = {
    assumeTrue("Assume NoModule", isNoModule)
    // Initialization
    assertEquals(5, global.TopLevelExport_basicVal)
    assertEquals("hello", global.TopLevelExport_basicVar)

    // Scala modifies var
    TopLevelFieldExports.basicVar = "modified"
    assertEquals("modified", TopLevelFieldExports.basicVar)
    assertEquals("modified", global.TopLevelExport_basicVar)

    // Reset var
    TopLevelFieldExports.basicVar = "hello"
  }

  @Test def topLevelExportBasicFieldModule(): AsyncResult = await {
    assumeFalse("Assume Module", isNoModule)
    for (exp <- exportsNamespace) yield {
      // Initialization
      assertEquals(5, exp.TopLevelExport_basicVal)
      assertEquals("hello", exp.TopLevelExport_basicVar)

      // Scala modifies var
      TopLevelFieldExports.basicVar = "modified"
      assertEquals("modified", TopLevelFieldExports.basicVar)
      assertEquals("modified", exp.TopLevelExport_basicVar)

      // Reset var
      TopLevelFieldExports.basicVar = "hello"
    }
  }

  @Test def topLevelExportFieldTwice(): Unit = {
    assumeTrue("Assume NoModule", isNoModule)

    // Initialization
    assertEquals(5, global.TopLevelExport_valExportedTwice1)
    assertEquals("hello", global.TopLevelExport_varExportedTwice1)
    assertEquals("hello", global.TopLevelExport_varExportedTwice2)

    // Scala modifies var
    TopLevelFieldExports.varExportedTwice = "modified"
    assertEquals("modified", TopLevelFieldExports.varExportedTwice)
    assertEquals("modified", global.TopLevelExport_varExportedTwice1)
    assertEquals("modified", global.TopLevelExport_varExportedTwice2)

    // Reset var
    TopLevelFieldExports.varExportedTwice = "hello"
  }

  @Test def topLevelExportFieldTwiceModule(): AsyncResult = await {
    assumeFalse("Assume Module", isNoModule)
    for (exp <- exportsNamespace) yield {
      // Initialization
      assertEquals(5, exp.TopLevelExport_valExportedTwice1)
      assertEquals("hello", exp.TopLevelExport_varExportedTwice1)
      assertEquals("hello", exp.TopLevelExport_varExportedTwice2)

      // Scala modifies var
      TopLevelFieldExports.varExportedTwice = "modified"
      assertEquals("modified", TopLevelFieldExports.varExportedTwice)
      assertEquals("modified", exp.TopLevelExport_varExportedTwice1)
      assertEquals("modified", exp.TopLevelExport_varExportedTwice2)

      // Reset var
      TopLevelFieldExports.varExportedTwice = "hello"
    }
  }

  @Test def topLevelExportWriteValVarCausesTypeerror(): AsyncResult = await {
    assumeFalse("Unchecked in Script mode", isNoModule)

    for (exp <- exportsNamespace) yield {
      assertThrows(classOf[js.JavaScriptException], {
        exp.TopLevelExport_basicVal = 54
      })

      assertThrows(classOf[js.JavaScriptException], {
        exp.TopLevelExport_basicVar = 54
      })
    }
  }

  @Test def topLevelExportUninitializedFieldsScala(): Unit = {
    assertEquals(0, TopLevelFieldExports.uninitializedVarInt)
    assertEquals(0L, TopLevelFieldExports.uninitializedVarLong)
    assertEquals(null, TopLevelFieldExports.uninitializedVarString)
    assertEquals('\u0000', TopLevelFieldExports.uninitializedVarChar)
  }

  @Test def topLevelExportUninitializedFields(): Unit = {
    assumeTrue("Assume NoModule", isNoModule)
    assertEquals(null, global.TopLevelExport_uninitializedVarInt)
    assertEquals(null, global.TopLevelExport_uninitializedVarLong)
    assertEquals(null, global.TopLevelExport_uninitializedVarString)
    assertEquals(null, global.TopLevelExport_uninitializedVarChar)
  }

  @Test def topLevelExportUninitializedFieldsModule(): AsyncResult = await {
    assumeFalse("Assume Module", isNoModule)
    for (exp <- exportsNamespace) yield {
      assertEquals(null, exp.TopLevelExport_uninitializedVarInt)
      assertEquals(null, exp.TopLevelExport_uninitializedVarLong)
      assertEquals(null, exp.TopLevelExport_uninitializedVarString)
      assertEquals(null, exp.TopLevelExport_uninitializedVarChar)
    }
  }

  @Test def topLevelExportFieldIsAlwaysReachableAndInitialized(): Unit = {
    assumeTrue("Assume NoModule", isNoModule)
    assertEquals("Hello World", global.TopLevelExport_fieldreachability)
  }

  @Test def topLevelExportFieldIsAlwaysReachableAndInitializedModule(): AsyncResult = await {
    assumeFalse("Assume Module", isNoModule)
    for (exp <- exportsNamespace) yield {
      assertEquals("Hello World", exp.TopLevelExport_fieldreachability)
    }
  }

  @Test def topLevelExportFieldIsWritableAccrossModules(): Unit = {
    /* We write to basicVar exported above from a different object to test writing
     * of static fields across module boundaries (when module splitting is
     * enabled).
     */

    assertEquals("hello", TopLevelFieldExports.inlineVar)
    TopLevelFieldExports.inlineVar = "hello modules"
    assertEquals("hello modules", TopLevelFieldExports.inlineVar)

    // Reset var
    TopLevelFieldExports.inlineVar = "hello"
  }

  // @JSExportTopLevel in Script's are `let`s

  @Test def topLevelExportsNoModuleAreLets(): Unit = {
    assumeTrue("relevant only for NoModule", isNoModule)

    val g = JSUtils.globalObject

    assertEquals(true, js.isUndefined(g.TopLevelExportedObject))
    assertEquals(true, js.isUndefined(g.SJSDefinedTopLevelExportedObject))
    assertEquals(true, js.isUndefined(g.TopLevelExportedClass))
    assertEquals(true, js.isUndefined(g.SJSDefinedTopLevelExportedClass))
    assertEquals(true, js.isUndefined(g.TopLevelExport_basic))
    assertEquals(true, js.isUndefined(g.TopLevelExport_basicVal))
    assertEquals(true, js.isUndefined(g.TopLevelExport_basicVar))
  }
}

object TopLevelExportNameHolder {
  final val className = "ConstantFoldedClassExport"
  final val objectName = "ConstantFoldedObjectExport"
}

/** Access to a `witness` property in instances of exported Scala classes. */
trait WitnessInterface {
  def witness: Any
}

@JSExportTopLevel("TopLevelExportedObject")
@JSExportTopLevel(TopLevelExportNameHolder.objectName)
object TopLevelExportedObject extends WitnessInterface {
  val witness: String = "witness"
}

@JSExportTopLevel("SJSDefinedTopLevelExportedObject")
object SJSDefinedExportedObject extends js.Object {
  val witness: String = "witness"
}

@JSExportTopLevel("ProtectedExportedObject")
protected object ProtectedExportedObject extends WitnessInterface {
  def witness: String = "witness"
}

@JSExportTopLevel("TopLevelExportedClass")
@JSExportTopLevel(TopLevelExportNameHolder.className)
class TopLevelExportedClass(_x: Int) extends WitnessInterface {
  val witness = _x
}

@JSExportTopLevel("SJSDefinedTopLevelExportedClass")
class SJSDefinedTopLevelExportedClass(val x: Int) extends js.Object

@JSExportTopLevel("TopLevelExportedAbstractJSClass")
abstract class TopLevelExportedAbstractJSClass(val x: Int) extends js.Object {
  def foo(y: Int): Int

  def bar(y: Int): Int = 3 * foo(y)
}

@JSExportTopLevel("ProtectedExportedClass")
protected class ProtectedExportedClass(_x: Int) extends WitnessInterface {
  val witness = _x
}

@JSExportTopLevel("ExportedVarArgClass")
class ExportedVarArgClass(x: String*) extends WitnessInterface {

  @JSExportTopLevel("ExportedVarArgClass")
  def this(x: Int, y: String) = this(s"Number: <$x>", y)

  def witness: String = x.mkString("|")
}

@JSExportTopLevel("ExportedDefaultArgClass")
class ExportedDefaultArgClass(x: Int, y: Int, z: Int) extends WitnessInterface {

  @JSExportTopLevel("ExportedDefaultArgClass")
  def this(x: Int, y: Int = 5) = this(x, y, 100)

  def witness: Int = x + y + z
}

object ExportHolder {
  @JSExportTopLevel("NestedExportedClass")
  class ExportedClass

  @JSExportTopLevel("NestedExportedObject")
  object ExportedObject

  @JSExportTopLevel("NestedSJSDefinedExportedClass")
  class SJSDefinedExportedClass extends js.Object
}

object TopLevelExports {
  @JSExportTopLevel("TopLevelExport_basic")
  def basic(): Int = 1

  @JSExportTopLevel("TopLevelExport_overload")
  def overload(x: String): String = "Hello " + x

  @JSExportTopLevel("TopLevelExport_overload")
  def overload(x: Int, y: Int*): Int = x + y.sum

  @JSExportTopLevel("TopLevelExport_defaultParams")
  def defaultParams(x: Int, y: Int = 1): Int = x + y

  var myVar: Int = _

  @JSExportTopLevel("TopLevelExport_set")
  def setMyVar(x: Int): Unit = myVar = x

  object Nested {
    var myVar: Int = _

    @JSExportTopLevel("TopLevelExport_setNested")
    def setMyVar(x: Int): Unit = myVar = x
  }

  @JSExportTopLevel("__topLevelExportWithDoubleUnderscore")
  val topLevelExportWithDoubleUnderscore: Boolean = true
}

/* This object is only reachable via the top level export to make sure the
 * analyzer behaves correctly.
 */
object TopLevelExportsReachability {
  private val name = "World"

  @JSExportTopLevel("TopLevelExport_reachability")
  def basic(): String = "Hello " + name
}

object TopLevelFieldExports {
  @JSExportTopLevel("TopLevelExport_basicVal")
  val basicVal: Int = 5

  @JSExportTopLevel("TopLevelExport_basicVar")
  var basicVar: String = "hello"

  @JSExportTopLevel("TopLevelExport_valExportedTwice1")
  @JSExportTopLevel("TopLevelExport_valExportedTwice2")
  val valExportedTwice: Int = 5

  @JSExportTopLevel("TopLevelExport_varExportedTwice1")
  @JSExportTopLevel("TopLevelExport_varExportedTwice2")
  var varExportedTwice: String = "hello"

  @JSExportTopLevel("TopLevelExport_uninitializedVarInt")
  var uninitializedVarInt: Int = _

  @JSExportTopLevel("TopLevelExport_uninitializedVarLong")
  var uninitializedVarLong: Long = _

  @JSExportTopLevel("TopLevelExport_uninitializedVarString")
  var uninitializedVarString: String = _

  @JSExportTopLevel("TopLevelExport_uninitializedVarChar")
  var uninitializedVarChar: Char = _

  // the export is only to make the field IR-static
  @JSExportTopLevel("TopLevelExport_irrelevant")
  @(inline @meta.getter @meta.setter)
  var inlineVar: String = "hello"
}

/* This object and its static initializer are only reachable via the top-level
 * export of its field, to make sure the analyzer and the static initiliazer
 * behave correctly.
 */
object TopLevelFieldExportsReachability {
  private val name = "World"

  @JSExportTopLevel("TopLevelExport_fieldreachability")
  val greeting = "Hello " + name
}
