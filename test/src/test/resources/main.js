/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, Jonas Fonseca    **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

console.log("Starting Scala.js test suite");
console.log("");

/* Load all test suites ... */
ScalaJS.modules.scala_scalajs_compiler_RegressionTest();
ScalaJS.modules.java_lang_ObjectTest();
ScalaJS.modules.java_lang_IntegerTest();
ScalaJS.modules.java_lang_StringTest();
ScalaJS.modules.java_util_ArraysTest();
ScalaJS.modules.scala_scalajs_js_ArrayTest();
ScalaJS.modules.scala_scalajs_js_DictionaryTest();
ScalaJS.modules.scala_scalajs_js_DynamicTest();
ScalaJS.modules.scala_EnumerationTest();

/* ... and run them. */
var jasmineEnv = jasmine.getEnv();
jasmineEnv.addReporter(new jasmine.RhinoReporter());
jasmineEnv.updateInterval = 0;
jasmineEnv.execute();
