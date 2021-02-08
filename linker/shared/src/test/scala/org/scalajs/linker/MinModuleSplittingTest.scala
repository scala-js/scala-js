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

package org.scalajs.linker

import scala.concurrent._

import org.junit.Test
import org.junit.Assert._

import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.junit.async._

import org.scalajs.linker.interface._
import org.scalajs.linker.testutils.LinkingUtils._
import org.scalajs.linker.testutils.TestIRBuilder._

class MinModuleSplittingTest {
  import scala.concurrent.ExecutionContext.Implicits.global

  /** Smoke test to ensure modules do not get merged too much. */
  @Test
  def splitsModules(): AsyncResult = await {
    val greetMethodName = m("greet", Nil, T)

    val greeterMemberDefs = List(
        trivialCtor("lib.Greeter"),

        // @noinline def greet(): String = "Hello world!"
        MethodDef(EMF, greetMethodName, NON, Nil, StringType, Some {
          str("Hello world!")
        })(EOH.withNoinline(true), None)
    )

    val classDefs = Seq(
        classDef("lib.Greeter",
            superClass = Some(ObjectClass),
            memberDefs = greeterMemberDefs
        ),

        mainTestClassDef({
            // console.log(new lib.Greeter().greet())
            val newGreeter = New("lib.Greeter", NoArgConstructorName, Nil)
            val callGreet = Apply(EAF, newGreeter, greetMethodName, Nil)(StringType)
            consoleLog(callGreet)
        })
    )

    val expectedFiles = Set(
        "java.lang.Object.js",
        "Test.js",
        "lib.Greeter.js",
        "main.js"
    )

    val linkerConfig = StandardConfig()
      .withModuleKind(ModuleKind.ESModule)
      .withModuleSplitStyle(ModuleSplitStyle.SmallestModules)
      .withSourceMap(false)

    val outputDirectory = MemOutputDirectory()

    for {
      _ <- testLink(classDefs, MainTestModuleInitializers,
          config = linkerConfig, output = outputDirectory)
    } yield {
      assertEquals(expectedFiles, outputDirectory.fileNames().toSet)
    }
  }
}
