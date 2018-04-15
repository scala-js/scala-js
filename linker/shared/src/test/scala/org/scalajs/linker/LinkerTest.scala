package org.scalajs.linker

import org.junit.Test
import org.junit.Assert._

import org.scalajs.ir.ClassKind
import org.scalajs.ir.Definitions._
import org.scalajs.ir.Trees._

import org.scalajs.logging._
import org.scalajs.io._

import org.scalajs.linker._
import org.scalajs.linker.irio._

import org.scalajs.linker.testutils._
import org.scalajs.linker.testutils.TestIRBuilder._

class LinkerTest {
  import LinkerTest._

  /** Makes sure that the minilib is sufficient to completely link a hello
   *  world.
   */
  @Test
  def linkHelloWorld(): Unit = {
    val name = "LHelloWorld$"
    val mainMethodBody = {
      JSBracketMethodApply(JSGlobalRef(Ident("console")), StringLiteral("log"),
          List(StringLiteral("Hello world!")))
    }
    val classDefs = Seq(
        classDef(name, kind = ClassKind.ModuleClass,
            superClass = Some(ObjectClass),
            memberDefs = List(
                trivialCtor(name),
                mainMethodDef(mainMethodBody)
            )
        )
    )
    val moduleInitializers = List(
        ModuleInitializer.mainMethodWithArgs("HelloWorld", "main")
    )
    testLink(classDefs, moduleInitializers)
  }

  /** This test exposes a problem where a linker in error state is called
   *  multiple times and ends up thinking it is being used concurrently.
   */
  @Test
  def clean_linking_state(): Unit = {
    class DummyException extends Exception

    val badSeq = new IndexedSeq[VirtualScalaJSIRFile] {
      def apply(x: Int): VirtualScalaJSIRFile = throw new DummyException()
      def length: Int = throw new DummyException()
    }

    val linker = StandardLinker(StandardLinker.Config())

    def callLink(): Unit = {
      val out = LinkerOutput(WritableMemVirtualBinaryFile("some_file"))
      linker.link(badSeq, Nil, out, NullLogger)
    }

    // Call first time. Get exception from badSeq.
    try {
      callLink()
      fail("Expected DummyException")
    } catch {
      case e: DummyException => // ok.
    }

    def callInFailedState(): Unit = {
      try {
        callLink()
        fail("Expected IllegalStateException")
      } catch {
        case e: IllegalStateException =>
          if (e.getMessage.contains("concurrent")) {
            fail("Found bad message in exception: " + e.getMessage)
          }
      }
    }

    for (_ <- 1 to 4) callInFailedState()
  }

}

object LinkerTest {
  def testLink(classDefs: Seq[ClassDef],
      moduleInitializers: List[ModuleInitializer]): Unit = {

    val linker = StandardLinker(StandardLinker.Config())

    val classDefsFiles = classDefs.map { classDef =>
      new VirtualScalaJSIRFile {
        def exists: Boolean = true
        def path: String = "mem://" + classDef.name.name + ".sjsir"
        def tree: ClassDef = classDef
        def relativePath: String = classDef.name.name + ".sjsir"
      }
    }

    val allIRFiles = TestIRRepo.stdlibIRFiles ++ classDefsFiles

    val output = LinkerOutput(WritableMemVirtualBinaryFile("output.js"))

    linker.link(allIRFiles, moduleInitializers, output,
        new ScalaConsoleLogger(Level.Error))
  }
}
