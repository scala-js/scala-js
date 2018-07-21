package org.scalajs.jsenv.test

import scala.concurrent.Await

import org.scalajs.io.VirtualBinaryFile
import org.scalajs.jsenv._

import org.junit.Assert._
import org.junit.Assume._
import org.junit.{Test, Before}

private[test] class RunTests(config: JSEnvSuiteConfig, withCom: Boolean) {
  private val kit = new TestKit(config, withCom)
  import kit._

  @Test
  def failureTest: Unit = {
    """
    var a = {};
    a.foo();
    """.fails()
  }

  @Test
  def syntaxErrorTest: Unit = {
    """
    {
    """.fails()
  }

  @Test // Failed in Phantom - #2053
  def utf8Test: Unit = {
    """
    console.log("\u1234");
    """ hasOutput "\u1234\n";
  }

  @Test
  def allowScriptTags: Unit = {
    """
    console.log("<script></script>");
    """ hasOutput "<script></script>\n";
  }

  @Test
  def jsExitsTest: Unit = {
    assumeTrue(config.supportsExit)

    val run = kit.start("__ScalaJSEnv.exitFunction(0);", RunConfig())
    try {
      Await.result(run.future, config.awaitTimeout)
    } finally {
      run.close()
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

    codes.mkString("").hasOutput(result)
  }

  @Test // Node.js console.log hack didn't allow to log non-Strings - #561
  def nonStringTest: Unit = {
    """
    console.log(1);
    console.log(undefined);
    console.log(null);
    console.log({});
    console.log([1,2]);
    """ hasOutput
    """|1
       |undefined
       |null
       |[object Object]
       |1,2
    |""".stripMargin
  }

  @Test
  def fastCloseTest: Unit = {
    /* This test also tests a failure mode where the ExternalJSRun is still
     * piping output while the client calls close.
     */
    val run = kit.start("", RunConfig())
    run.close()
    awaitAfterClose(run)
  }

  @Test
  def multiCloseAfterTerminatedTest: Unit = {
    val run = kit.start("", RunConfig())
    run.close()
    awaitAfterClose(run)

    // Should be noops (and not fail).
    run.close()
    run.close()
    run.close()
  }

  @Test
  def noThrowOnBadFileTest: Unit = {
    val badFile = new VirtualBinaryFile {
      def path: String = ???
      def exists: Boolean = ???
      def inputStream: java.io.InputStream = ???
    }

    // `start` may not throw but must fail asynchronously
    val run = kit.start(badFile, RunConfig())
    try {
      Await.ready(run.future, config.awaitTimeout)
      assertTrue("Bad file should have made run fail",
          run.future.value.get.isFailure)
    } finally {
      run.close()
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
    kit.start("", cfg).close()
  }
}
