package build

import sbt._

import org.scalajs.ir.ScalaJSVersions

object ScalaJSEnvGenerator {

  private final val relPath = "linker/scalajsenv.js"

  /** Generate a *.scala file that contains the scalajsenv as literal string
   *
   *  We need this so the tools don't rely on I/O and/or resources.
   */
  def generateEnvHolder(projectRoot: File, sourceDir: File): Seq[File] = {
    val trg = sourceDir / "ScalaJSEnvHolder.scala"
    val env = projectRoot / relPath

    val sourceMapPath = {
      if (!ScalaJSVersions.currentIsSnapshot)
        s"https://raw.githubusercontent.com/scala-js/scala-js/v${ScalaJSVersions.current}/$relPath"
      else
        env.getAbsoluteFile.toURI.toASCIIString
    }

    if (!trg.exists() || trg.lastModified() < env.lastModified()) {
      val scalajsenv = IO.read(env).replaceAllLiterally("$", "$$")

      val scalaCode =
        s"""
        package org.scalajs.linker.backend.emitter

        import java.net.URI

        private[emitter] object ScalaJSEnvHolder {
          final val scalajsenv = raw\"\"\"$scalajsenv\"\"\"
          final val sourceMapPath = new URI(raw\"\"\"$sourceMapPath\"\"\")
        }
        """

      IO.write(trg, scalaCode)
    }

    Seq(trg)
  }

}
