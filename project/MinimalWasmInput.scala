package build

import java.nio.file.Path

import org.scalajs.jsenv.Input

object MinimalWasmInput {
  final case class MinimalWasmModule(module: Path) extends Input
}
