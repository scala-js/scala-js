package customlinker

import java.nio.file.Path

import org.scalajs.linker._
import org.scalajs.linker.interface._
import org.scalajs.linker.standard._

object CustomLinkerImpl {
  def linker(config: StandardConfig, entryPointOutputFile: Path): Linker = {
    val frontend = StandardLinkerFrontend(config)
    val backend = new EntryPointAnalyzerBackend(config, entryPointOutputFile)
    StandardLinkerImpl(frontend, backend)
  }

  def clearableLinker(config: StandardConfig,
      entryPointOutputFile: Path): ClearableLinker = {
    ClearableLinker(() => linker(config, entryPointOutputFile),
        config.batchMode)
  }
}
