package org.scalajs.testcommon

/** Mode in which the test interface executes. */
private[scalajs] sealed abstract class TestInterfaceMode

private[scalajs] object TestInterfaceMode {
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
