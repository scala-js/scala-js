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
