package envvars

object Platform {
  def getEnvVarOpt(envVar: String): Option[String] =
    Option(System.getenv(envVar))
}
