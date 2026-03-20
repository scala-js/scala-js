package org.scalajs.jsenv

import java.nio.file.Path

object MinimalWasmInput {
  final case class MinimalWasmModule(module: Path) extends Input
}
