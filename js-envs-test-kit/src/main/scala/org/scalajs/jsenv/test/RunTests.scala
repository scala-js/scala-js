package org.scalajs.jsenv.test

import org.junit.Assume._
import org.junit.{Test, Before}

import org.scalajs.io.VirtualBinaryFile
import org.scalajs.jsenv._
import org.scalajs.jsenv.test.kit.{TestKit, Run}

private[test] class RunTests(config: JSEnvSuiteConfig, withCom: Boolean) {
  private val kit = new TestKit(config.jsEnv, config.awaitTimeout)

  private def withRun(input: Input)(body: Run => Unit) = {
    if (withCom) kit.withComRun(input)(body)
    else kit.withRun(input)(body)
  }

  private def withRun(code: String, config: RunConfig = RunConfig())(body: Run => Unit) = {
    if (withCom) kit.withComRun(code, config)(body)
    else kit.withRun(code, config)(body)
  }

  @Test
  def failureTest: Unit = {
    withRun("""
      var a = {};
      a.foo();
    """) {
      _.fails()
    }
  }

  @Test
  def syntaxErrorTest: Unit = {
    withRun("{") {
      _.fails()
    }
  }

  @Test
  def throwExceptionTest: Unit = {
    withRun("throw 1;") {
      _.fails()
    }
  }

  @Test
  def catchExceptionTest: Unit = {
    withRun("""
      try {
        throw "hello world";
      } catch (e) {
        console.log(e);
      }
    """) {
      _.expectOut("hello world\n")
        .closeRun()
    }
  }

  @Test // Failed in Phantom - #2053
  def utf8Test: Unit = {
    withRun("""console.log("\u1234")""") {
      _.expectOut("\u1234\n")
        .closeRun()
    }
  }

  @Test
  def allowScriptTags: Unit = {
    withRun("""console.log("<script></script>");""") {
      _.expectOut("<script></script>\n")
        .closeRun()
    }
  }

  @Test
  def jsExitsTest: Unit = {
    assumeTrue(config.supportsExit)

    withRun("__ScalaJSEnv.exitFunction(0);") {
      _.succeeds()
    }
  }

  @Test // Node.js strips double percentage signs - #500
  def percentageTest: Unit = {
    val counts = 1 to 15
    val argcs  = 1 to 3
    val strings = counts.map("%" * _)

    val strlists = for {
      count  <- argcs
      string <- strings
    } yield List.fill(count)(string)

    val codes = for {
      strlist <- strlists
    } yield {
      val args = strlist.map(s => s""""$s"""").mkString(", ")
      s"console.log($args);\n"
    }

    val result = strlists.map(_.mkString(" ") + "\n").mkString("")

    withRun(codes.mkString("")) {
      _.expectOut(result)
        .closeRun()
    }
  }

  @Test // Node.js console.log hack didn't allow to log non-Strings - #561
  def nonStringTest: Unit = {
    withRun("""
      console.log(1);
      console.log(undefined);
      console.log(null);
      console.log({});
      console.log([1,2]);
      """) {
      _.expectOut("1\n")
        .expectOut("undefined\n")
        .expectOut("null\n")
        .expectOut("[object Object]\n")
        .expectOut("1,2\n")
        .closeRun()
    }
  }

  @Test
  def fastCloseTest: Unit = {
    /* This test also tests a failure mode where the ExternalJSRun is still
     * piping output while the client calls close.
     */
    withRun("") {
      _.closeRun()
    }
  }

  @Test
  def multiCloseAfterTerminatedTest: Unit = {
    withRun("") { run =>
      run.closeRun()

      // Should be noops (and not fail).
      run.closeRun()
      run.closeRun()
      run.closeRun()
    }
  }

  @Test
  def noThrowOnBadFileTest: Unit = {
    def fail() = throw new java.io.IOException("exception for test")

    val badFile = new VirtualBinaryFile {
      def path: String = fail()
      def exists: Boolean = fail()
      def inputStream: java.io.InputStream = fail()
    }

    // `start` may not throw but must fail asynchronously
    withRun(Input.ScriptsToLoad(badFile :: Nil)) {
      _.fails()
    }
  }

  /* This test verifies that a [[JSEnv]] properly validates its [[RunConfig]]
   * (through [[RunConfig.Validator]]).
   *
   * If you get here, because the test suite fails on your [[JSEnv]] you are not
   * using [[RunConfig.Validator]] properly (or at all). See its documentation
   * on how to use it properly.
   *
   * This test sets a private option on [[RunConfig]] that is only known
   * internally. This ensures that [[JSEnv]]s reject options added in the future
   * they cannot support.
   */
  @Test(expected = classOf[IllegalArgumentException])
  def ensureValidate: Unit = {
    val cfg = RunConfig().withEternallyUnsupportedOption(true)
    withRun("", cfg)(identity)
  }
}
