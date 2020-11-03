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
import org.scalajs.nscplugin.test.util.VersionDependentUtils.scalaSupportsNoWarn

import org.junit.Assume._
import org.junit.Test

// scalastyle:off line.size.limit

class StaticForwardersWarningsAllObjectsTest extends DirectTest with TestHelpers {

  override def extraArgs: List[String] =
    super.extraArgs ::: List("-P:scalajs:genStaticForwardersForNonTopLevelObjects")

  @Test
  def warnWhenAvoidingStaticForwardersForNonTopLevelObject: Unit = {
    """
    object Enclosing {
      class A

      object a {
        def foo(x: Int): Int = x + 1
      }
    }
    """.hasWarns("""
      |newSource1.scala:5: warning: Not generating the static forwarders of Enclosing$a because its name differs only in case from the name of another class or trait in this compilation unit.
      |      object a {
      |             ^
    """)
  }

  @Test
  def noWarnIfSelectivelyDisabled: Unit = {
    assumeTrue(scalaSupportsNoWarn)

    """
    import scala.annotation.nowarn

    object Enclosing {
      class A

      @nowarn("cat=other")
      object a {
        def foo(x: Int): Int = x + 1
      }
    }
    """.hasNoWarns()
  }

}
