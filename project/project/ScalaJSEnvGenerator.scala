import sbt._

object ScalaJSEnvGenerator {

  /** Generate a *.scala file that contains the scalajsenv as literal string
   *
   *  We need this so the tools don't rely on I/O and/or resources.
   */
  def generateEnvHolder(baseDir: File, sourceDir: File): Seq[File] = {
    val trg = sourceDir / "ScalaJSEnvHolder.scala"
    val env = baseDir / "scalajsenv.js"

    if (!trg.exists() || trg.lastModified() < env.lastModified()) {
      val scalajsenv = IO.read(env).replaceAllLiterally("$", "$$")

      val scalaCode =
        s"""
        package org.scalajs.core.tools.linker.backend.emitter

        private[emitter] object ScalaJSEnvHolder {
          final val scalajsenv = raw\"\"\"$scalajsenv\"\"\"
        }
        """

      IO.write(trg, scalaCode)
    }

    Seq(trg)
  }

}
