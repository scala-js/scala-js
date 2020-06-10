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
import org.scalajs.ir.EntryPointsInfo
import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._

import org.scalajs.logging._

import org.scalajs.junit.async._

import org.scalajs.linker.interface._
import org.scalajs.linker.testutils._
import org.scalajs.linker.testutils.TestIRBuilder._

class EmitterTest {
  import scala.concurrent.ExecutionContext.Implicits.global

  @Test
  def testLinkingErrorForImportInScript(): AsyncResult = await {
    val nativeMemberName = m("nativeMember", Nil, O)

    val classDefs = Seq(
        classDef(
            "Imported",
            kind = ClassKind.NativeJSModuleClass,
            superClass = Some(ObjectClass),
            jsNativeLoadSpec = Some(JSNativeLoadSpec.Import("foo.js", List("Foo")))
        ),
        classDef(
            "WithImportMember",
            kind = ClassKind.ModuleClass,
            superClass = Some(ObjectClass),
            memberDefs = List(
                JSNativeMemberDef(EMF.withNamespace(MemberNamespace.PublicStatic),
                    nativeMemberName,
                    JSNativeLoadSpec.Import("bar.js", List("Bar")))
            )
        ),
        mainTestClassDef(Block(
            consoleLog(LoadJSModule("Imported")),
            consoleLog(SelectJSNativeMember("WithImportMember", nativeMemberName))
        ))
    )

    val linkResult = LinkingUtils.expectFailure(LinkingUtils.linkAndEmit(
        classDefs,
        MainTestModuleInitializers,
        StandardConfig().withModuleKind(ModuleKind.NoModule)))

    for (result <- linkResult) yield {
      val exception = result.exception
      assertTrue(
          exception.getMessage(),
          exception.getMessage().startsWith(
              "There were module imports without fallback to global " +
              "variables, but module support is disabled."))

      val log = result.log
      log.assertContainsLogLine(
          "Imported needs to be imported from module 'foo.js' but module " +
          "support is disabled.")
      log.assertContainsLogLine(
          "WithImportMember.nativeMember()java.lang.Object needs to be " +
          "imported from module 'bar.js' but module support is disabled.")
    }
  }
}
