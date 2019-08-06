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

package org.scalajs.jsenv.test

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import com.google.common.jimfs.Jimfs

import org.junit.Assume._
import org.junit.{Test, Before, AssumptionViolatedException}

import org.scalajs.jsenv._
import org.scalajs.jsenv.test.kit.{TestKit, Run}

private[test] class RunTests(config: JSEnvSuiteConfig, withCom: Boolean) {
  private val kit = new TestKit(config.jsEnv, config.awaitTimeout)

  private def withRun(input: Seq[Input])(body: Run => Unit) = {
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
    val exitStat = config.exitJSStatement.getOrElse(
      throw new AssumptionViolatedException("JSEnv needs exitJSStatement"))

    withRun(exitStat) {
      _.succeeds()
    }
  }

  // #500 Node.js used to strip double percentage signs even with only 1 argument
  @Test
  def percentageTest: Unit = {
    val strings = (1 to 15).map("%" * _)
    val code = strings.map(str => s"""console.log("$str");\n""").mkString("")
    val result = strings.mkString("", "\n", "\n")

    withRun(code) {
      _.expectOut(result)
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
    val badFile = Jimfs.newFileSystem().getPath("nonexistent")

    // `start` may not throw but must fail asynchronously
    withRun(Input.Script(badFile) :: Nil) {
      _.fails()
    }
  }

  @Test
  def defaultFilesystem: Unit = {
    // Tests that a JSEnv works with files from the default filesystem.

    val tmpFile = File.createTempFile("sjs-run-test-defaultfile", ".js")
    try {
      val tmpPath = tmpFile.toPath
      Files.write(tmpPath, "console.log(\"test\");".getBytes(StandardCharsets.UTF_8))

      withRun(Input.Script(tmpPath) :: Nil) {
        _.expectOut("test\n")
          .closeRun()
      }
    } finally {
      tmpFile.delete()
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
