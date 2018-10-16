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

package org.scalajs.testing.common

/** Mode in which the test interface executes. */
private[testing] sealed abstract class TestInterfaceMode

private[testing] object TestInterfaceMode {
  case object FullBridge extends TestInterfaceMode
  case class HTMLRunner(tests: IsolatedTestSet) extends TestInterfaceMode

  implicit object TestInterfaceModeSerializer extends Serializer[TestInterfaceMode] {
    def serialize(x: TestInterfaceMode, out: Serializer.SerializeState): Unit = x match {
      case FullBridge =>
        out.write(0)

      case HTMLRunner(tests) =>
        out.write(1)
        out.write(tests)
    }

    def deserialize(in: Serializer.DeserializeState): TestInterfaceMode = {
      in.read[Int]() match {
        case 0 => FullBridge
        case 1 => HTMLRunner(in.read[IsolatedTestSet]())
        case n => throw new java.io.IOException(s"Unknown interface mode: $n")
      }
    }
  }
}
