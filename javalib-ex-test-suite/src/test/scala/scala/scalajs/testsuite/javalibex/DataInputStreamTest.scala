package scala.scalajs.testsuite.javalibex

import java.io._

import org.scalajs.jasminetest.JasmineTest

import scala.scalajs.js.typedarray._
import scala.scalajs.js.JSConverters._

object DataInputStreamTest extends JasmineTest {

  def tests(name: String)(inFromBytes: Seq[Byte] => InputStream): Unit = {
    def newStream(data: Int*) =
      new DataInputStream(inFromBytes(data.map(_.toByte)))

    when("typedarray").
    describe(name) {

      it("should provide `readBoolean`") {
        val data = Seq(0x00, 0x01, 0xF1, 0x00, 0x01)
        val stream = newStream(data: _*)

        for (d <- data)
          expect(stream.readBoolean()).toBe(d != 0)

        expect(() => stream.readBoolean()).toThrow // EOF
      }

      it("should provide `readByte`") {
        val data = Seq(0x00, 0x01, 0xF1, 0x7D, 0x35)
        val stream = newStream(data: _*)

        for (d <- data)
          expect(stream.readByte()).toBe(d.toByte)

        expect(() => stream.readBoolean()).toThrow // EOF
      }

      it("should provide `readChar`") {
        val stream = newStream(
          0x00, 0x48, // H
          0x00, 0xF6, // ö
          0x00, 0x6C, // l
          0x00, 0x6C, // l
          0x00, 0xF6, // ö
          0x00, 0x20, // [space]
          0x00, 0x57, // W
          0x01, 0x03, // ă
          0x00, 0x72, // r
          0x02, 0x34, // ȴ
          0x01, 0x11, // đ
          0x56 // dangling
        )
        var res = ""

        for (i <- 1 to 11)
          res += stream.readChar()

        expect(res).toEqual("Höllö Wărȴđ")

        expect(() => stream.readChar()).toThrow // Dangling + EOF
      }

      it("should provide `readDouble`") {
        val stream = newStream(
            0x3f, 0xe6, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66,
            0x41, 0x15, 0x19, 0x20, 0x45, 0x8d, 0x9b, 0x5f,
            0xc0, 0xab, 0x20, 0x22, 0x75, 0x25, 0x46, 0x0b,
            0x7f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x7f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0xc0, 0x1c, 0x0d, 0xca, 0x65, 0xea, 0x3f, 0xa4,
            0xff, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x01 // dangling
        )

        expect(stream.readDouble()).toBe(0.7)
        expect(stream.readDouble()).toBe(345672.067923)
        expect(stream.readDouble()).toBe(-3472.0673)
        expect(stream.readDouble().isNaN).toBe(true)
        expect(stream.readDouble()).toBe(Double.PositiveInfinity)
        expect(stream.readDouble()).toBe(-7.0134674)
        expect(stream.readDouble()).toBe(Double.NegativeInfinity)
        expect(stream.readDouble()).toBe(0)
        expect(() => stream.readDouble()).toThrow
      }

      it("should provide `readFloat`") {
        val stream = newStream(
            0xbf, 0x80, 0x00, 0x00,
            0x45, 0x8e, 0x9c, 0x83,
            0x80, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0x7f, 0xc0, 0x00, 0x00,
            0x7f, 0x80, 0x00, 0x00,
            0xbb, 0x03, 0x12, 0x6f,
            0xff, 0x80, 0x00, 0x00,
            0xff // dangling
        )

        expect(stream.readFloat()).toBeCloseTo(-1.0f, 6)
        expect(stream.readFloat()).toBeCloseTo(4563.564f, 6)
        expect(stream.readFloat()).toBeCloseTo(-0.0f, 6)
        expect(stream.readFloat()).toBeCloseTo(0.0f, 6)
        expect(stream.readFloat().isNaN).toBe(true)
        expect(stream.readFloat()).toBe(Float.PositiveInfinity)
        expect(stream.readFloat()).toBeCloseTo(-0.002f, 6)
        expect(stream.readFloat()).toBe(Float.NegativeInfinity)
        expect(() => stream.readDouble()).toThrow
      }

      it("should provide `readInt`") {
        val stream = newStream(
            0x00, 0x00, 0x00, 0x00,
            0x7f, 0xff, 0xff, 0xff,
            0xff, 0xff, 0xff, 0xfc,
            0x00, 0x00, 0x00, 0x53,
            0x00, 0x00, 0x89, 0xa2,
            0xff, 0xfe, 0x82, 0xfd,
            0x80, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x01)

        expect(stream.readInt()).toBe(0)
        expect(stream.readInt()).toBe(Int.MaxValue)
        expect(stream.readInt()).toBe(-4)
        expect(stream.readInt()).toBe(83)
        expect(stream.readInt()).toBe(35234)
        expect(stream.readInt()).toBe(-97539)
        expect(stream.readInt()).toBe(Int.MinValue)
        expect(stream.readInt()).toBe(1)
        expect(() => stream.readDouble()).toThrow
      }

      it("should provide `readLong`") {
        val stream = newStream(
            0x00, 0x01, 0xf0, 0xec, 0x59, 0x0c, 0x70, 0x9a,
            0xff, 0xff, 0xff, 0xff, 0xfe, 0x10, 0xd5, 0x5e,
            0x7f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
            0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x79, 0x24,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x2f)

        expect(stream.readLong() == 546372873646234L).toBeTruthy
        expect(stream.readLong() == -32451234L).toBeTruthy
        expect(stream.readLong() == Long.MaxValue).toBeTruthy
        expect(stream.readLong() == 0L).toBeTruthy
        expect(stream.readLong() == -1L).toBeTruthy
        expect(stream.readLong() == Long.MinValue).toBeTruthy
        expect(stream.readLong() == -34524L).toBeTruthy
        expect(stream.readLong() == 47L).toBeTruthy
        expect(() => stream.readDouble()).toThrow
      }

      it("should provide `readShort`") {
        val stream = newStream(
            0x01, 0xc5,
            0xff, 0xd5,
            0x7f, 0xff,
            0x18, 0xb0,
            0x00, 0x00,
            0x80, 0x00,
            0xfe, 0xa6,
            0x00, 0x22,
            0x01 // dangling
        )

        expect(stream.readShort()).toBe(453)
        expect(stream.readShort()).toBe(-43)
        expect(stream.readShort()).toBe(Short.MaxValue)
        expect(stream.readShort()).toBe(6320)
        expect(stream.readShort()).toBe(0)
        expect(stream.readShort()).toBe(Short.MinValue)
        expect(stream.readShort()).toBe(-346)
        expect(stream.readShort()).toBe(34)
        expect(() => stream.readDouble()).toThrow
      }

      it("should provide `readUnsignedByte`") {
        val data = Seq(0x00, 0x01, 0xF1, 0x7D, 0x35)
        val stream = newStream(data: _*)

        for (d <- data)
          expect(stream.readUnsignedByte()).toBe(d)

        expect(() => stream.readBoolean()).toThrow // EOF
      }

      it("should provide `readUnsignedShort`") {
        val stream = newStream(
            0xfe, 0x4c,
            0x00, 0x00,
            0x18, 0xee,
            0x0d, 0xed,
            0x00, 0x2b,
            0x01, 0xce,
            0x01, 0x56,
            0x64, 0x2b,
            0x01 // dangling
        )

        expect(stream.readUnsignedShort()).toBe(65100)
        expect(stream.readUnsignedShort()).toBe(0)
        expect(stream.readUnsignedShort()).toBe(6382)
        expect(stream.readUnsignedShort()).toBe(3565)
        expect(stream.readUnsignedShort()).toBe(43)
        expect(stream.readUnsignedShort()).toBe(462)
        expect(stream.readUnsignedShort()).toBe(342)
        expect(stream.readUnsignedShort()).toBe(25643)
        expect(() => stream.readDouble()).toThrow
      }

      it("should provide `readFully` (1 arg & 3 arg)") {
        val stream = newStream(-100 to 99: _*)
        val buf = new Array[Byte](50)

        stream.readFully(buf)
        expect(buf.toJSArray).toEqual((-100 to -51).toJSArray)

        expect(() => stream.readFully(null)).toThrow

        stream.readFully(buf, 40, 10)
        expect(buf.toJSArray).toEqual(((-100 to -61) ++ (-50 to -41)).toJSArray)

        expect(() => stream.readFully(buf, 70, 1)).toThrow
        expect(() => stream.readFully(buf, 10, 100)).toThrow
        expect(() => stream.readFully(buf, -1, 2)).toThrow
        expect(() => stream.readFully(buf, 0, -1)).toThrow

        stream.readFully(buf, 0, 50)
        expect(buf.toJSArray).toEqual((-40 to 9).toJSArray)

        stream.readFully(buf, 0, 50)
        expect(buf.toJSArray).toEqual((10 to 59).toJSArray)

        expect(() => stream.readFully(buf)).toThrow
      }

      it("should provide `readFully` for bursty streams") {
        class BurstyStream(length: Int, burst: Int) extends InputStream {
          private var i: Int = 0
          def read(): Int = if (i < length) { i += 1; i } else -1
          override def read(buf: Array[Byte], off: Int, reqLen: Int): Int = {
            val len = Math.min(Math.min(reqLen, burst), length - i)
            if (reqLen == 0) 0
            else if (len == 0) -1
            else {
              var j: Int = 0
              while (j < len) {
                buf(off+j) = read().toByte
                j += 1
              }
              len
            }
          }
        }

        val stream = new DataInputStream(new BurstyStream(100, 5))
        val buf = new Array[Byte](50)

        stream.readFully(buf)
        expect(buf.toJSArray).toEqual((1 to 50).toJSArray)

        stream.readFully(buf, 40, 10)
        expect(buf.toJSArray).toEqual(((1 to 40) ++ (51 to 60)).toJSArray)

        expect(() => stream.readFully(buf)).toThrow
      }

      it("should provide `readUTF`") {
        val stream = newStream(
            0x00, 0x10, 0x48, 0xc3, 0xb6, 0x6c, 0x6c, 0xc3,
            0xb6, 0x20, 0x57, 0xc4, 0x83, 0x72, 0xc8, 0xb4,
            0xc4, 0x91, 0x00, 0x0d, 0x70, 0x6f, 0x6f, 0x20,
            0x2d, 0x3e, 0x20, 0xed, 0xa0, 0xbd, 0xed, 0xb2,
            0xa9, 0x00, 0x03, 0xe6, 0x84, 0x9b)

        expect(stream.readUTF).toEqual("Höllö Wărȴđ")
        expect(stream.readUTF).toEqual("poo -> 💩")
        expect(stream.readUTF).toEqual("愛")

        val badStream = newStream(0x00, 0x01, 0xC0, 0x82)
        expect(() => badStream.readUTF).toThrow
      }

      it("should provide `readLine`") {
        val stream = newStream(
            "Hello World\nUNIX\nWindows\r\nMac (old)\rStuff".map(_.toInt): _*)

        expect(stream.readLine()).toEqual("Hello World")
        expect(stream.readLine()).toEqual("UNIX")
        expect(stream.readLine()).toEqual("Windows")
        expect(stream.readLine()).toEqual("Mac (old)")
        expect(stream.readLine()).toEqual("Stuff")
        expect(stream.readLine()).toBe(null)
      }

      it("should allow marking even when `readLine` has to push back") {
        val stream = newStream(
            "Hello World\nUNIX\nWindows\r\nMac (old)\rStuff".map(_.toInt): _*)

        expect(stream.readLine()).toEqual("Hello World")
        stream.mark(1000)
        expect(stream.readLine()).toEqual("UNIX")
        stream.reset()
        expect(stream.readLine()).toEqual("UNIX")
        expect(stream.readLine()).toEqual("Windows")
        expect(stream.readLine()).toEqual("Mac (old)")
        stream.mark(1000)
        expect(stream.readLine()).toEqual("Stuff")
        stream.reset()
        expect(stream.readLine()).toEqual("Stuff")
        expect(stream.readLine()).toBe(null)
      }

    }

  }

  tests("java.io.DataInputStream - generic case")(bytes =>
    new ByteArrayInputStream(bytes.toArray))

  tests("java.io.DataInputStream - ArrayBufferInputStream case")(bytes =>
    new ArrayBufferInputStream(new Int8Array(bytes.toJSArray).buffer))

  tests("java.io.DataInputStream - partially consumed ArrayBufferInputStream case") { bytes =>
    val addBytes = Seq[Byte](0, 0, 0, 0)
    val allBytes = addBytes ++ bytes
    val in = new ArrayBufferInputStream(
        new Int8Array(allBytes.toJSArray).buffer)

    for (_ <- addBytes) in.read()

    in
  }

}
