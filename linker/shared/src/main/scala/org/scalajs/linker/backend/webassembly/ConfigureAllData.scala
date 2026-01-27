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

package org.scalajs.linker.backend.webassembly

/** Structures to represent the "data" argument to `"wasm:js-prototypes" "configureAll"`.
 *
 *  @see
 *    [[https://github.com/WebAssembly/custom-descriptors/blob/main/proposals/custom-descriptors/Overview.md]]
 */
object ConfigureAllData {
  final case class Data(protoConfigs: List[ProtoConfig])

  final case class ProtoConfig(
      constructorConfigs: Option[ConstructorConfig],
      methodConfigs: List[MethodConfig],
      parentIndex: Option[Int]
  )

  final case class ConstructorConfig(
      constructorName: String,
      staticMethodConfigs: List[MethodConfig]
  )

  sealed abstract class MethodConfig

  object MethodConfig {
    final case class Method(name: String) extends MethodConfig
    final case class Getter(name: String) extends MethodConfig
    final case class Setter(name: String) extends MethodConfig
  }
}
