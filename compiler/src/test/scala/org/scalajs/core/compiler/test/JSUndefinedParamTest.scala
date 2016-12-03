package org.scalajs.core.compiler.test

import org.scalajs.core.compiler.test.util._
import org.junit.Test

// scalastyle:off line.size.limit

/** This tests the UndefinedParam tracker in the compiler backend.
 *
 *  In order to properly implement removal of trailing default parameters, the
 *  compiler backend may generate a UndefinedParam tree and catch it later.
 *  However, some macros and compiler plugins may generate trees the backend
 *  doesn't expect. As a result the backend used to generate invalid IR.
 *  Instead, we now track these helper trees and emit a more helpful error
 *  message if one of them sticks around.
 *
 *  This test contains a macro that generates a tree we cannot handle and
 *  verifies that the compiler bails out.
 */
class JSUndefinedParamTest extends DirectTest with TestHelpers {

  /* We need a macro in the test. Therefore, we add scala-reflect and the
   * compiler's output path itself to the classpath.
   */
  override def classpath: List[String] =
    super.classpath ++ List(scalaReflectPath, testOutputPath)

  @Test def noDanglingUndefinedParam: Unit = {

    // Define macro that extracts method parameter.
    """
    import language.experimental.macros

    /** Dummy object to get the right shadowing for cross compilation */
    private object Compat210 {
      object blackbox { // scalastyle:ignore
        type Context = scala.reflect.macros.Context
      }
    }

    import Compat210._

    object JSUndefinedParamTest {
      import scala.reflect.macros._ // shadows blackbox from above
      import blackbox.Context

      def extractArg(call: Any): Any = macro extractArg_impl

      def extractArg_impl(c: Context)(call: c.Expr[Any]): c.Expr[Any] = {
        import c.universe._

        call.tree match {
          case Apply(fun, List(arg)) => c.Expr[Any](arg)

          case tree =>
            c.abort(tree.pos, "Bad tree. Need function call with single argument.")
        }
      }
    }
    """.succeeds()

    // Use the macro to trigger UndefinedParam catcher.
    """
    import scala.scalajs.js
    import scala.scalajs.js.annotation._

    @js.native
    trait MyTrait extends js.Any {
      def foo(x: Int = js.native): Int = js.native
    }

    object A {
      val myTrait: MyTrait = ???

      /* We assign the default parameter value for foo to b.
       * This should fail.
       */
      val b = JSUndefinedParamTest.extractArg(myTrait.foo())
    }
    """ hasErrors
    """
      |newSource1.scala:10: error: Found a dangling UndefinedParam at Position(virtualfile:newSource1.scala,15,54). This is likely due to a bad interaction between a macro or a compiler plugin and the Scala.js compiler plugin. If you hit this, please let us know.
      |    object A {
      |           ^
    """

  }

}
