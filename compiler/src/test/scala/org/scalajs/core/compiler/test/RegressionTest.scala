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
import org.junit.Assume._

// scalastyle:off line.size.limit

class RegressionTest extends DirectTest with TestHelpers {

  @Test
  def noCrashWhenCallingDefaultMethodsOfCharSequence_issue3211: Unit = {
    val javaVersion = System.getProperty("java.specification.version")
    assumeTrue(javaVersion.startsWith("1.8") || !javaVersion.startsWith("1."))

    """
    object LexerUtil {
      def reflectiveLongest(data: String): Unit = {
        println(data.chars())
      }
    }
    """.succeeds()

    """
    import scala.language.reflectiveCalls

    object LexerUtil {
      def reflectiveLongest(data: Any { def chars: String }): Unit = {
        println(data.chars)
      }
    }
    """.succeeds()
  }

}
