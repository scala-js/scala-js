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
            NoConfigMain,
            TestConfigMain2,
            TestConfigMain1,
            TestConfigMainArgs1 + "()",
            TestConfigMainArgs2 + "(foo, bar)"
        ),
        moduleInitializersEffects.toArray[AnyRef])
  }

}

object ModuleInitializersTest {
  final val NoConfigMain = "ModuleInitializerInNoConfiguration.main"
  final val CompileConfigMain = "ModuleInitializerInCompileConfiguration.main"
  final val TestConfigMain1 = "ModuleInitializerInTestConfiguration.main1"
  final val TestConfigMain2 = "ModuleInitializerInTestConfiguration.main2"
  final val TestConfigMainArgs1 = "ModuleInitializerInTestConfiguration.mainArgs1"
  final val TestConfigMainArgs2 = "ModuleInitializerInTestConfiguration.mainArgs2"

  val moduleInitializersEffects =
    new scala.collection.mutable.ListBuffer[String]
}

object ModuleInitializerInNoConfiguration {
  import ModuleInitializersTest._

  def main(): Unit = {
    moduleInitializersEffects += NoConfigMain
  }
}

object ModuleInitializerInCompileConfiguration {
  import ModuleInitializersTest._

  def main(): Unit = {
    // This is not going to be actually run
    moduleInitializersEffects += CompileConfigMain
  }
}

object ModuleInitializerInTestConfiguration {
  import ModuleInitializersTest._

  def main1(): Unit = {
    moduleInitializersEffects += TestConfigMain1
  }

  def main2(): Unit = {
    moduleInitializersEffects += TestConfigMain2
  }

  def mainArgs1(args: Array[String]): Unit = {
    moduleInitializersEffects +=
      TestConfigMainArgs1 + args.mkString("(", ", ", ")")
  }

  def mainArgs2(args: Array[String]): Unit = {
    moduleInitializersEffects +=
      TestConfigMainArgs2 + args.mkString("(", ", ", ")")
  }
}
