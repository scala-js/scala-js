package org.scalajs.core.compiler.test

import util._

import org.junit.Test

import org.scalajs.core.ir.{Trees => js, Types => jstpe}

class OptimizationTest extends JSASTTest {

  @Test
  def unwrapScalaFunWrapper: Unit = {

    // Make sure we do not wrap and unwrap right away
    """
    import scala.scalajs.js

    class A {
      val jsFun: js.Function = (x: Int) => x * 2
    }
    """.
    hasNot("runtime.AnonFunction ctor") {
      case js.New(jstpe.ClassType("sjsr_AnonFunction1"), _, _) =>
    }

    // Make sure our wrapper matcher has the right name
    """
    import scala.scalajs.js

    class A {
      val scalaFun = (x: Int) => x * 2
      val jsFun: js.Function = scalaFun
    }
    """.
    has("runtime.AnonFunction ctor") {
      case js.New(jstpe.ClassType("sjsr_AnonFunction1"), _, _) =>
    }

    /* Make sure js.Array(...) is optimized away completely for several kinds
     * of data types.
     */
    """
    import scala.scalajs.js

    class VC(val x: Int) extends AnyVal

    class A {
      val a = js.Array(5, 7, 9, -3)
      val b = js.Array("hello", "world")
      val c = js.Array('a', 'b')
      val d = js.Array(Nil)
      val e = js.Array(new VC(151189))
    }
    """.
    hasNot("any of the wrapArray methods") {
      case js.Apply(_, js.Ident(name, _), _)
          if name.startsWith("wrap") && name.endsWith("__scm_WrappedArray") =>
    }

    /* Make sure varargs are optimized to use js.WrappedArray instead of
     * scm.WrappedArray, for various data types.
     */
    """
    import scala.scalajs.js

    class VC(val x: Int) extends AnyVal

    class A {
      val a = List(5, 7, 9, -3)
      val b = List("hello", "world")
      val c = List('a', 'b')
      val d = List(Nil)
      val e = List(new VC(151189))
    }
    """.
    hasNot("any of the wrapArray methods") {
      case js.Apply(_, js.Ident(name, _), _)
          if name.startsWith("wrap") && name.endsWith("__scm_WrappedArray") =>
    }

    // Make sure our wrapper matcher has the right name
    """
    import scala.scalajs.js

    class A {
      val a: Seq[Int] = new Array[Int](5)
    }
    """.
    has("one of the wrapArray methods") {
      case js.Apply(_, js.Ident(name, _), _)
          if name.startsWith("wrap") && name.endsWith("__scm_WrappedArray") =>
    }

    // Verify the optimized emitted code for 'new js.Object' and 'new js.Array'
    """
    import scala.scalajs.js

    class A {
      val o = new js.Object
      val a = new js.Array
    }
    """.
    hasNot("any reference to the global scope") {
      case js.JSLinkingInfo() =>
    }

  }

  @Test
  def switchWithoutGuards: Unit = {
    """
    class Test {
      def switchWithGuardsStat(x: Int, y: Int): Unit = {
        x match {
          case 1            => println("one")
          case 2            => println("two")
          case z if y > 100 => println("big " + z)
          case _            => println("None of those")
        }
      }
    }
    """.hasNot("Labeled block") {
      case js.Labeled(_, _, _) =>
    }.has("Match node") {
      case js.Match(_, _, _) =>
    }
  }

  @Test
  def switchWithGuards: Unit = {
    // Statement position
    """
    class Test {
      def switchWithGuardsStat(x: Int, y: Int): Unit = {
        x match {
          case 1            => println("one")
          case 2 if y < 10  => println("two special")
          case 2            => println("two")
          case 3 if y < 10  => println("three special")
          case 3 if y > 100 => println("three big special")
          case z if y > 100 => println("big " + z)
          case _            => println("None of those")
        }
      }
    }
    """.hasExactly(1, "default case (\"None of those\")") {
      case js.StringLiteral("None of those") =>
    }.has("Match node") {
      case js.Match(_, _, _) =>
    }

    // Expression position
    """
    class Test {
      def switchWithGuardsExpr(x: Int, y: Int): Unit = {
        val message = x match {
          case 1            => "one"
          case 2 if y < 10  => "two special"
          case 2            => "two"
          case 3 if y < 10  => "three special"
          case 3 if y > 100 => "three big special"
          case z if y > 100 => "big " + z
          case _            => "None of those"
        }
        println(message)
      }
    }
    """.hasExactly(1, "default case (\"None of those\")") {
      case js.StringLiteral("None of those") =>
    }.has("Match node") {
      case js.Match(_, _, _) =>
    }
  }

}
