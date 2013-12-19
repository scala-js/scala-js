/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

console.log("Starting Scala.js test suite");
console.log("");

/* Load all test suites ... */
ScalaJS.modules.scala_scalajs_test_compiler_InteroperabilityTest();
ScalaJS.modules.scala_scalajs_test_compiler_RegressionTest();
ScalaJS.modules.scala_scalajs_test_compiler_LongTest();

ScalaJS.modules.scala_scalajs_test_javalib_ObjectTest();
ScalaJS.modules.scala_scalajs_test_javalib_IntegerTest();
ScalaJS.modules.scala_scalajs_test_javalib_StringTest();
ScalaJS.modules.scala_scalajs_test_javalib_ArraysTest();
ScalaJS.modules.scala_scalajs_test_javalib_LongTest();

ScalaJS.modules.scala_scalajs_test_scalalib_EnumerationTest();

ScalaJS.modules.scala_scalajs_test_jsinterop_ArrayTest();
ScalaJS.modules.scala_scalajs_test_jsinterop_DictionaryTest();
ScalaJS.modules.scala_scalajs_test_jsinterop_DynamicTest();
ScalaJS.modules.scala_scalajs_test_jsinterop_RuntimeLongTest();

/* ... and run them. */
var jasmineEnv = jasmine.getEnv();
jasmineEnv.addReporter(new jasmine.RhinoReporter());
jasmineEnv.updateInterval = 0;
jasmineEnv.execute();

if (jasmineEnv.currentRunner().results().failedCount > 0)
  throw new Error("Some tests failed")
