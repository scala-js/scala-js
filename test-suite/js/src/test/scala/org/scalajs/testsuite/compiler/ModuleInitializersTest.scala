/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2017, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.compiler

import org.junit.Test
import org.junit.Assert._

class ModuleInitializersTest {
  import ModuleInitializersTest._

  @Test def correctInitializers(): Unit = {
    assertArrayEquals(
        Array[AnyRef](
            NoArgs,
            WithArgs + "()",
            WithArgs + "(foo, bar)"
        ),
        moduleInitializersEffects.toArray[AnyRef])
  }
}

object ModuleInitializersTest {
  final val NoArgs = "NoArgs"
  final val WithArgs = "WithArgs"

  val moduleInitializersEffects =
    new scala.collection.mutable.ListBuffer[String]
}

object ModuleInitializers {
  import ModuleInitializersTest._

  def mainNoArgs(): Unit =
    moduleInitializersEffects += NoArgs

  def mainWithArgs(args: Array[String]): Unit =
    moduleInitializersEffects += WithArgs + args.mkString("(", ", ", ")")
}
