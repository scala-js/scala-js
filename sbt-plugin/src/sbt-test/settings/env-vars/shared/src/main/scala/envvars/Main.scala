package envvars

object Main {
  def main(args: Array[String]): Unit = {
    assertEnvVar("scoped in project", "PROJECT_ENV_VAR")
    assertNoEnvVar("RUN_ENV_VAR")
    assertEnvVar("scoped in project/Compile", "COMPILE_ENV_VAR")
    assertEnvVar("scoped in project/Compile/run", "COMPILE_RUN_ENV_VAR")
    assertNoEnvVar("TEST_ENV_VAR")
    assertNoEnvVar("TEST_TEST_ENV_VAR")
  }

  private def assertNoEnvVar(envVar: String): Unit =
    assertEnvVar(None, envVar)

  private def assertEnvVar(expected: String, envVar: String): Unit =
    assertEnvVar(Some(expected), envVar)

  private def assertEnvVar(expected: Option[String], envVar: String): Unit = {
    val value = Platform.getEnvVarOpt(envVar)
    assert(value == expected, s"for $envVar, expected: $expected, but got: $value")
  }
}
