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

package org.scalajs.core.compiler.test

import org.scalajs.core.compiler.test.util._
import org.junit.Test

// scalastyle:off line.size.limit

class MissingJSGlobalDeprecationsTest extends DirectTest with TestHelpers {

  override def preamble: String =
    """import scala.scalajs.js, js.annotation._
    """

  @Test
  def warnNoAnnotClass: Unit = {
    """
    @js.native
    class A extends js.Object

    @js.native
    abstract class B extends js.Object
    """ hasWarns
    """
      |newSource1.scala:4: warning: Top-level native JS classes and objects should have an @JSGlobal or @JSImport annotation. This will be enforced in 1.0.
      |  If migrating from 0.6.14 or earlier, the equivalent behavior is an @JSGlobal without parameter.
      |  (you can suppress this warning in 0.6.x by passing the option `-P:scalajs:suppressMissingJSGlobalDeprecations` to scalac)
      |    class A extends js.Object
      |          ^
      |newSource1.scala:7: warning: Top-level native JS classes and objects should have an @JSGlobal or @JSImport annotation. This will be enforced in 1.0.
      |  If migrating from 0.6.14 or earlier, the equivalent behavior is an @JSGlobal without parameter.
      |  (you can suppress this warning in 0.6.x by passing the option `-P:scalajs:suppressMissingJSGlobalDeprecations` to scalac)
      |    abstract class B extends js.Object
      |                   ^
    """
  }

  @Test
  def warnNoAnnotObject: Unit = {
    """
    @js.native
    object A extends js.Object
    """ hasWarns
    """
      |newSource1.scala:4: warning: Top-level native JS classes and objects should have an @JSGlobal or @JSImport annotation. This will be enforced in 1.0.
      |  If migrating from 0.6.14 or earlier, the equivalent behavior is an @JSGlobal without parameter.
      |  (you can suppress this warning in 0.6.x by passing the option `-P:scalajs:suppressMissingJSGlobalDeprecations` to scalac)
      |    object A extends js.Object
      |           ^
    """
  }

  @Test
  def warnJSNameClass: Unit = {
    """
    @js.native
    @JSName("Foo")
    class A extends js.Object

    @js.native
    @JSName("Foo")
    abstract class B extends js.Object
    """ hasWarns
    """
      |newSource1.scala:4: warning: @JSName on top-level native JS classes and objects (or native JS classes and objects inside Scala objects) is deprecated, and should be replaced by @JSGlobal (with the same meaning). This will be enforced in 1.0.
      |  (you can suppress this warning in 0.6.x by passing the option `-P:scalajs:suppressMissingJSGlobalDeprecations` to scalac)
      |    @JSName("Foo")
      |     ^
      |newSource1.scala:8: warning: @JSName on top-level native JS classes and objects (or native JS classes and objects inside Scala objects) is deprecated, and should be replaced by @JSGlobal (with the same meaning). This will be enforced in 1.0.
      |  (you can suppress this warning in 0.6.x by passing the option `-P:scalajs:suppressMissingJSGlobalDeprecations` to scalac)
      |    @JSName("Foo")
      |     ^
    """
  }

  @Test
  def warnJSNameObject: Unit = {
    """
    @js.native
    @JSName("Foo")
    object A extends js.Object
    """ hasWarns
    """
      |newSource1.scala:4: warning: @JSName on top-level native JS classes and objects (or native JS classes and objects inside Scala objects) is deprecated, and should be replaced by @JSGlobal (with the same meaning). This will be enforced in 1.0.
      |  (you can suppress this warning in 0.6.x by passing the option `-P:scalajs:suppressMissingJSGlobalDeprecations` to scalac)
      |    @JSName("Foo")
      |     ^
    """
  }

  @Test
  def warnJSNameNestedClass: Unit = {
    """
    object Enclosing {
      @js.native
      @JSName("Foo")
      class A extends js.Object

      @js.native
      @JSName("Foo")
      abstract class B extends js.Object
    }
    """ hasWarns
    """
      |newSource1.scala:5: warning: @JSName on top-level native JS classes and objects (or native JS classes and objects inside Scala objects) is deprecated, and should be replaced by @JSGlobal (with the same meaning). This will be enforced in 1.0.
      |  (you can suppress this warning in 0.6.x by passing the option `-P:scalajs:suppressMissingJSGlobalDeprecations` to scalac)
      |      @JSName("Foo")
      |       ^
      |newSource1.scala:9: warning: @JSName on top-level native JS classes and objects (or native JS classes and objects inside Scala objects) is deprecated, and should be replaced by @JSGlobal (with the same meaning). This will be enforced in 1.0.
      |  (you can suppress this warning in 0.6.x by passing the option `-P:scalajs:suppressMissingJSGlobalDeprecations` to scalac)
      |      @JSName("Foo")
      |       ^
    """
  }

  @Test
  def warnJSNameNestObject: Unit = {
    """
    object Enclosing {
      @js.native
      @JSName("Foo")
      object A extends js.Object
    }
    """ hasWarns
    """
      |newSource1.scala:5: warning: @JSName on top-level native JS classes and objects (or native JS classes and objects inside Scala objects) is deprecated, and should be replaced by @JSGlobal (with the same meaning). This will be enforced in 1.0.
      |  (you can suppress this warning in 0.6.x by passing the option `-P:scalajs:suppressMissingJSGlobalDeprecations` to scalac)
      |      @JSName("Foo")
      |       ^
    """
  }

}
