/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.test
package compiler

/** Tests the little reflection we support */
object ReflectionTest extends JasmineTest {

  describe("Scala.js Reflection") {
    it("should append $ to class name of objects") {
      expect(TestObject.getClass.getName).toEqual(
        "scala.scalajs.test.compiler.ReflectionTest$TestObject$")
    }
  }

  object TestObject

}
