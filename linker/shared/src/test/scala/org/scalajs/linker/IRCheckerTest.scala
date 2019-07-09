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

import org.scalajs.ir.ClassKind
import org.scalajs.ir.Definitions._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.logging._

import org.scalajs.junit.async._

import org.scalajs.linker._
import org.scalajs.linker.standard._

import org.scalajs.linker.testutils._
import org.scalajs.linker.testutils.TestIRBuilder._

class IRCheckerTest {
  import scala.concurrent.ExecutionContext.Implicits.global

  import IRCheckerTest._

  @Test
  def testMethodCallOnClassWithNoInstances(): AsyncResult = await {
    def callMethOn(receiver: Tree): Tree =
      Apply(EAF, receiver, Ident("meth__LFoo__V"), List(Null()))(NoType)

    val classDefs = Seq(
        // LFoo will be dropped by base linking
        classDef("LFoo", superClass = Some(ObjectClass)),

        classDef("LBar",
            superClass = Some(ObjectClass),
            memberDefs = List(
                trivialCtor("LBar"),

                /* This method is called, but unreachable because there are no
                 * instances of `Bar`. It will therefore not make `Foo` reachable.
                 */
                MethodDef(MemberFlags.empty, Ident("meth__LFoo__V"),
                    List(paramDef("foo", ClassType("LFoo"))), NoType,
                    Some(Skip()))(
                    emptyOptHints, None)
            )
        ),

        classDef("LTest$", kind = ClassKind.ModuleClass,
            superClass = Some(ObjectClass),
            memberDefs = List(
                trivialCtor("LTest$"),
                MethodDef(MemberFlags.empty, Ident("nullBar__LBar"), Nil, ClassType("LBar"),
                    Some(Null()))(
                    emptyOptHints, None),
                mainMethodDef(Block(
                    callMethOn(Apply(EAF, This()(ClassType("LTest$")),
                        Ident("nullBar__LBar"), Nil)(ClassType("LBar"))),
                    callMethOn(Null()),
                    callMethOn(Throw(Null()))
                ))
            )
        )
    )

    testLinkNoIRError(classDefs, mainModuleInitializers("Test"))
  }

}

object IRCheckerTest {
  def testLinkNoIRError(classDefs: Seq[ClassDef],
      moduleInitializers: List[ModuleInitializer])(
      implicit ec: ExecutionContext): Future[Unit] = {

    val config = StandardLinker.Config()
      .withCheckIR(true)
      .withOptimizer(false)
    val linkerFrontend = StandardLinkerFrontend(config)
    val symbolRequirements = StandardLinkerBackend(config).symbolRequirements

    val classDefsFiles = classDefs.map(MemClassDefIRFile(_))

    val result = TestIRRepo.minilib.stdlibIRFiles.flatMap { stdLibFiles =>
      linkerFrontend.link(stdLibFiles ++ classDefsFiles, moduleInitializers,
        symbolRequirements, new ScalaConsoleLogger(Level.Error))
    }

    result.map(_ => ())
  }
}
