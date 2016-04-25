/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.noircheck

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import scala.scalajs.js

class RhinoLinkFailureTest {

  @Test def Rhino_linking_should_throw_an_exception_if_it_fails_loading_a_class(): Unit = {
    val executingInRhino = System.getProperty("scalajs.rhino", "false") == "true"
    assumeTrue("Assumed executing in Rhino", executingInRhino)

    // scala.collection.parallel.Splitter$ is not defined
    try {
      val pool = scala.collection.parallel.Splitter.empty
      sys.error("Should not reach here")
    } catch {
      case js.JavaScriptException(e) =>
        // Make sure offending class is reported
        assertTrue(e.toString.contains("sc_parallel_Splitter$"))
    }

  }

}
