package envvars

import org.junit.Test
import org.junit.Assert._

class TestSuite {
  @Test
  def testEnvVars(): Unit = {
    assertEnvVar("scoped in project", "PROJECT_ENV_VAR")
    assertNoEnvVar("RUN_ENV_VAR")
    assertEnvVar("scoped in project/Compile", "COMPILE_ENV_VAR") // because Test "extends" Compile
    assertNoEnvVar("COMPILE_RUN_ENV_VAR")
    assertEnvVar("scoped in project/Test", "TEST_ENV_VAR")
    assertNoEnvVar("TEST_TEST_ENV_VAR") // always ignored
  }

  private def assertNoEnvVar(envVar: String): Unit =
    assertEnvVar(None, envVar)

  private def assertEnvVar(expected: String, envVar: String): Unit =
    assertEnvVar(Some(expected), envVar)

  private def assertEnvVar(expected: Option[String], envVar: String): Unit = {
    val value = Platform.getEnvVarOpt(envVar)
    assertEquals(s"for $envVar", expected, value)
  }
}
