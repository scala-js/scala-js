package build

import java.nio.file.Path

import org.scalajs.jsenv.Input

object WasmInput {
  final case class WasmModule(module: Path) extends Input
}
