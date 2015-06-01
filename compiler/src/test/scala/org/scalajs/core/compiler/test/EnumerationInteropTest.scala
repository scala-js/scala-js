package org.scalajs.core.compiler.test

import org.scalajs.core.compiler.test.util._

import org.junit.Test

class EnumerationInteropTest extends DirectTest with TestHelpers {

  @Test
  def warnIfUnableToTransformValue: Unit = {

    """
    class A extends Enumeration {
      val a = {
        println("oh, oh!")
        Value
      }
      val b = {
        println("oh, oh!")
        Value(4)
      }
    }
    """ hasWarns
    """
      |newSource1.scala:5: warning: Couldn't transform call to Enumeration.Value.
      |The resulting program is unlikely to function properly as this
      |operation requires reflection.
      |        Value
      |        ^
      |newSource1.scala:9: warning: Couldn't transform call to Enumeration.Value.
      |The resulting program is unlikely to function properly as this
      |operation requires reflection.
      |        Value(4)
      |             ^
    """

  }

  @Test
  def warnIfNoNameVal: Unit = {

    """
    class A extends Enumeration {
      val a = new Val
      val b = new Val(10)
    }
    """ hasWarns
    """
      |newSource1.scala:3: warning: Calls to the non-string constructors of Enumeration.Val
      |require reflection at runtime. The resulting
      |program is unlikely to function properly.
      |      val a = new Val
      |              ^
      |newSource1.scala:4: warning: Calls to the non-string constructors of Enumeration.Val
      |require reflection at runtime. The resulting
      |program is unlikely to function properly.
      |      val b = new Val(10)
      |              ^
    """

  }

  @Test
  def warnIfNullValue: Unit = {

    """
    class A extends Enumeration {
      val a = Value(null)
      val b = Value(10, null)
    }
    """ hasWarns
    """
      |newSource1.scala:3: warning: Passing null as name to Enumeration.Value
      |requires reflection at runtime. The resulting
      |program is unlikely to function properly.
      |      val a = Value(null)
      |                   ^
      |newSource1.scala:4: warning: Passing null as name to Enumeration.Value
      |requires reflection at runtime. The resulting
      |program is unlikely to function properly.
      |      val b = Value(10, null)
      |                   ^
    """

  }

  @Test
  def warnIfNullNewVal: Unit = {

    """
    class A extends Enumeration {
      val a = new Val(null)
      val b = new Val(10, null)
    }
    """ hasWarns
    """
      |newSource1.scala:3: warning: Passing null as name to a constructor of Enumeration.Val
      |requires reflection at runtime. The resulting
      |program is unlikely to function properly.
      |      val a = new Val(null)
      |              ^
      |newSource1.scala:4: warning: Passing null as name to a constructor of Enumeration.Val
      |requires reflection at runtime. The resulting
      |program is unlikely to function properly.
      |      val b = new Val(10, null)
      |              ^
    """

  }

  @Test
  def warnIfExtNoNameVal: Unit = {

    """
    class A extends Enumeration {
      protected class Val1 extends Val
      protected class Val2 extends Val(1)
    }
    """ warns() // no message checking: position differs in 2.10 and 2.11

  }

  @Test
  def warnIfExtNullNameVal: Unit = {

    """
    class A extends Enumeration {
      protected class Val1 extends Val(null)
      protected class Val2 extends Val(1,null)
    }
    """ warns() // no message checking: position differs in 2.10 and 2.11

  }

}
