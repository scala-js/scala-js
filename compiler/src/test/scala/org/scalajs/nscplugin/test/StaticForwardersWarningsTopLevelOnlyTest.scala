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

package org.scalajs.nscplugin.test

import org.scalajs.nscplugin.test.util._
import org.scalajs.nscplugin.test.util.VersionDependentUtils._

import org.junit.Assume._
import org.junit.Test

// scalastyle:off line.size.limit

class StaticForwardersWarningsTopLevelOnlyTest extends DirectTest with TestHelpers {

  @Test
  def warnWhenAvoidingStaticForwardersForTopLevelObject: Unit = {
    val jvmBackendIssuesWarningOfItsOwn = {
      scalaVersion != "2.12.1" &&
      scalaVersion != "2.12.2" &&
      scalaVersion != "2.12.3" &&
      scalaVersion != "2.12.4"
    }
    val jvmBackendMessage = if (!jvmBackendIssuesWarningOfItsOwn) {
      ""
    } else {
      """
        |newSource1.scala:4: warning: Generated class a differs only in case from A.
        |  Such classes will overwrite one another on case-insensitive filesystems.
        |    object a {
        |           ^
      """
    }

    """
    class A

    object a {
      def foo(x: Int): Int = x + 1
    }
    """ hasWarns
    s"""
      |newSource1.scala:4: warning: Not generating the static forwarders of a because its name differs only in case from the name of another class or trait in this compilation unit.
      |    object a {
      |           ^$jvmBackendMessage
    """
  }

  @Test
  def noWarnIfSelectivelyDisabled: Unit = {
    assumeTrue(scalaSupportsNoWarn)

    """
    import scala.annotation.nowarn

    class A

    @nowarn("cat=other")
    object a {
      def foo(x: Int): Int = x + 1
    }
    """.hasNoWarns()
  }

  @Test
  def noWarnForNonTopLevelObject: Unit = {
    """
    object Enclosing {
      class A

      object a {
        def foo(x: Int): Int = x + 1
      }
    }
    """.hasNoWarns()
  }

}
