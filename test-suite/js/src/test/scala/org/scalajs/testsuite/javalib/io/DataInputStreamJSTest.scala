package org.scalajs.testsuite.javalib.io

import java.io.InputStream

import org.scalajs.testsuite.utils.Platform._

import scala.scalajs.js.typedarray._
import scala.scalajs.js.JSConverters._

import org.junit._
import org.junit.Assume._

trait AssumeTypedArrays {
  @BeforeClass def assumeTypedArrays(): Unit = {
    assumeTrue("Assumed typed arrays", typedArrays)
  }
}

object DataInputStreamArrayBufferInputStreamTest extends AssumeTypedArrays

class DataInputStreamArrayBufferInputStreamTest extends DataInputStreamTest {
  protected def inFromBytes(bytes: Seq[Byte]): InputStream =
    new ArrayBufferInputStream(new Int8Array(bytes.toJSArray).buffer)
}

object DataInputStreamArrayPartiallyConsumedArrayBufferInputStreamTest
    extends AssumeTypedArrays

class DataInputStreamArrayPartiallyConsumedArrayBufferInputStreamTest
    extends DataInputStreamTest {

  protected def inFromBytes(bytes: Seq[Byte]): InputStream = {
    val addBytes = Seq[Byte](0, 0, 0, 0)
    val allBytes = addBytes ++ bytes
    val in = new ArrayBufferInputStream(
        new Int8Array(allBytes.toJSArray).buffer)

    for (_ <- addBytes)
      in.read()

    in
  }
}
