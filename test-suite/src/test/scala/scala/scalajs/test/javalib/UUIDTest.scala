/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.test.javalib

import scala.scalajs.test.JasmineTest

import java.util.UUID

object UUIDTest extends JasmineTest {
  describe("java.util.UUID") {
    it("constructor") {
      val uuid = new UUID(0xf81d4fae7dec11d0L, 0xa76500a0c91e6bf6L)
      expect(uuid.getMostSignificantBits() == 0xf81d4fae7dec11d0L).toBeTruthy
      expect(uuid.getLeastSignificantBits() == 0xa76500a0c91e6bf6L).toBeTruthy
      expect(uuid.variant()).toEqual(2)
      expect(uuid.version()).toEqual(1)
      expect(uuid.timestamp() == 0x1d07decf81d4faeL).toBeTruthy
      expect(uuid.clockSequence()).toEqual(0x2765)
      expect(uuid.node() == 0xA0C91E6BF6L).toBeTruthy
    }

    it("getLeastSignificantBits") {
      expect(new UUID(0L, 0L).getLeastSignificantBits() == 0L).toBeTruthy
      expect(new UUID(0L, Long.MinValue).getLeastSignificantBits() == Long.MinValue).toBeTruthy
      expect(new UUID(0L, Long.MaxValue).getLeastSignificantBits() == Long.MaxValue).toBeTruthy
    }

    it("getMostSignificantBits") {
      expect(new UUID(0L, 0L).getMostSignificantBits() == 0L).toBeTruthy
      expect(new UUID(Long.MinValue, 0L).getMostSignificantBits() == Long.MinValue).toBeTruthy
      expect(new UUID(Long.MaxValue, 0L).getMostSignificantBits() == Long.MaxValue).toBeTruthy
    }

    it("version") {
      expect(new UUID(0L, 0L).version()).toEqual(0)
      expect(new UUID(0x0000000000001000L, 0L).version()).toEqual(1)
      expect(new UUID(0x00000000000f2f00L, 0L).version()).toEqual(2)
    }

    it("variant") {
      expect(new UUID(0L, 0L).variant()).toEqual(0)
      expect(new UUID(0L, 0x7000000000000000L).variant()).toEqual(0)
      expect(new UUID(0L, 0x3ff0000000000000L).variant()).toEqual(0)
      expect(new UUID(0L, 0x1ff0000000000000L).variant()).toEqual(0)

      expect(new UUID(0L, 0x8000000000000000L).variant()).toEqual(2)
      expect(new UUID(0L, 0xb000000000000000L).variant()).toEqual(2)
      expect(new UUID(0L, 0xaff0000000000000L).variant()).toEqual(2)
      expect(new UUID(0L, 0x9ff0000000000000L).variant()).toEqual(2)

      expect(new UUID(0L, 0xc000000000000000L).variant()).toEqual(6)
      expect(new UUID(0L, 0xdf00000000000000L).variant()).toEqual(6)
    }

    it("timestamp") {
      expect(new UUID(0x0000000000001000L,
          0x8000000000000000L).timestamp() == 0L).toBeTruthy
      expect(new UUID(0x7777777755551333L,
          0x8000000000000000L).timestamp() == 0x333555577777777L).toBeTruthy

      expect(() => new UUID(0x0000000000000000L, 0x8000000000000000L).timestamp()).toThrow
      expect(() => new UUID(0x0000000000002000L, 0x8000000000000000L).timestamp()).toThrow
    }

    it("clockSequence") {
      expect(new UUID(0x0000000000001000L, 0x8000000000000000L).clockSequence()).toEqual(0)
      expect(new UUID(0x0000000000001000L, 0x8fff000000000000L).clockSequence()).toEqual(0x0fff)
      expect(new UUID(0x0000000000001000L, 0xBfff000000000000L).clockSequence()).toEqual(0x3fff)

      expect(() => new UUID(0x0000000000000000L, 0x8000000000000000L).clockSequence()).toThrow
      expect(() => new UUID(0x0000000000002000L, 0x8000000000000000L).clockSequence()).toThrow
    }

    it("node") {
      expect(new UUID(0x0000000000001000L, 0x8000000000000000L).node() == 0L).toBeTruthy
      expect(new UUID(0x0000000000001000L, 0x8000ffffffffffffL).node() == 0xffffffffffffL).toBeTruthy

      expect(() => new UUID(0x0000000000000000L, 0x8000000000000000L).node()).toThrow
      expect(() => new UUID(0x0000000000002000L, 0x8000000000000000L).node()).toThrow
    }

    it("compareTo") {
      val uuid0101 = new UUID(1L, 1L)
      val uuid0111 = new UUID(1L, 0x100000001L)
      val uuid1000 = new UUID(0x100000000L, 0L)

      expect(uuid0101.compareTo(uuid0101)).toEqual(0)
      expect(uuid0111.compareTo(uuid0111)).toEqual(0)
      expect(uuid1000.compareTo(uuid1000)).toEqual(0)

      expect(uuid0101.compareTo(uuid0111)).toBeLessThan(0)
      expect(uuid0101.compareTo(uuid1000)).toBeLessThan(0)
      expect(uuid0111.compareTo(uuid1000)).toBeLessThan(0)

      expect(uuid0111.compareTo(uuid0101)).toBeGreaterThan(0)
      expect(uuid1000.compareTo(uuid0101)).toBeGreaterThan(0)
      expect(uuid1000.compareTo(uuid0111)).toBeGreaterThan(0)
    }

    it("hashCode") {
      expect(new UUID(0L, 0L).hashCode()).toEqual(0)
      expect(new UUID(123L, 123L).hashCode()).toEqual(new UUID(123L, 123L).hashCode())
    }

    it("equals") {
      val uuid1 = new UUID(0L, 0L)
      expect(uuid1.equals(uuid1)).toBeTruthy
      expect(uuid1.equals(null)).toBeFalsy
      expect(uuid1.equals("something else")).toBeFalsy

      val uuid2 = new UUID(0L, 0L)
      expect(uuid1.equals(uuid2)).toBeTruthy

      val uuid3 = new UUID(0xf81d4fae7dec11d0L, 0xa76500a0c91e6bf6L)
      val uuid4 = new UUID(0xf81d4fae7dec11d0L, 0xa76500a0c91e6bf6L)
      expect(uuid3.equals(uuid4)).toBeTruthy
      expect(uuid3.equals(uuid1)).toBeFalsy

      expect(uuid3.equals(new UUID(0x781d4fae7dec11d0L, 0xa76500a0c91e6bf6L))).toBeFalsy
      expect(uuid3.equals(new UUID(0xf81d4fae7dec11d1L, 0xa76500a0c91e6bf6L))).toBeFalsy
      expect(uuid3.equals(new UUID(0xf81d4fae7dec11d0L, 0xa76530a0c91e6bf6L))).toBeFalsy
      expect(uuid3.equals(new UUID(0xf81d4fae7dec11d0L, 0xa76500a0c91e6cf6L))).toBeFalsy
    }

    it("toString") {
      expect(new UUID(0xf81d4fae7dec11d0L,
          0xa76500a0c91e6bf6L).toString()).toEqual("f81d4fae-7dec-11d0-a765-00a0c91e6bf6")
      expect(new UUID(0x0000000000001000L,
          0x8000000000000000L).toString()).toEqual("00000000-0000-1000-8000-000000000000")
    }

    it("randomUUID") {
      val uuid = UUID.randomUUID()
      expect(uuid.variant()).toEqual(2)
      expect(uuid.version()).toEqual(4)
    }

    it("fromString") {
      val uuid1 = UUID.fromString("f81d4fae-7dec-11d0-a765-00a0c91e6bf6")
      expect(uuid1.equals(new UUID(0xf81d4fae7dec11d0L, 0xa76500a0c91e6bf6L))).toBeTruthy
      expect(uuid1.getMostSignificantBits() == 0xf81d4fae7dec11d0L).toBeTruthy
      expect(uuid1.getLeastSignificantBits() == 0xa76500a0c91e6bf6L).toBeTruthy
      expect(uuid1.variant()).toEqual(2)
      expect(uuid1.version()).toEqual(1)
      expect(uuid1.timestamp() == 130742845922168750L).toBeTruthy
      expect(uuid1.clockSequence()).toEqual(10085)
      expect(uuid1.node() == 690568981494L).toBeTruthy

      val uuid2 = UUID.fromString("00000000-0000-1000-8000-000000000000")
      expect(uuid2.equals(new UUID(0x0000000000001000L, 0x8000000000000000L)))
      println(java.lang.Long.toHexString(uuid2.getMostSignificantBits()))
      println(java.lang.Long.toHexString(uuid2.getLeastSignificantBits()))
      expect(uuid2.getMostSignificantBits() == 0x0000000000001000L).toBeTruthy
      expect(uuid2.getLeastSignificantBits() == 0x8000000000000000L).toBeTruthy
      expect(uuid2.variant()).toEqual(2)
      expect(uuid2.version()).toEqual(1)
      expect(uuid2.timestamp() == 0L).toBeTruthy
      expect(uuid2.clockSequence()).toEqual(0)
      expect(uuid2.node() == 0L).toBeTruthy

      expect(() => UUID.fromString(null)).toThrow
      expect(() => UUID.fromString("")).toThrow
      expect(() => UUID.fromString("f81d4fae_7dec-11d0-a765-00a0c91e6bf6")).toThrow
      expect(() => UUID.fromString("f81d4fae-7dec_11d0-a765-00a0c91e6bf6")).toThrow
      expect(() => UUID.fromString("f81d4fae-7dec-11d0_a765-00a0c91e6bf6")).toThrow
      expect(() => UUID.fromString("f81d4fae-7dec-11d0-a765_00a0c91e6bf6")).toThrow
      expect(() => UUID.fromString("-7dec-11d0-a765-00a0c91e6bf6")).toThrow
      expect(() => UUID.fromString("f81d4fae--11d0-a765-00a0c91e6bf6")).toThrow
      expect(() => UUID.fromString("f81d4fae-7dec--a765-00a0c91e6bf6")).toThrow
      expect(() => UUID.fromString("f81d4fae-7dec-11d0--00a0c91e6bf6")).toThrow
      expect(() => UUID.fromString("f81d4fae-7dec-11d0-a765-")).toThrow
      expect(() => UUID.fromString("f81d4fae-7dec-11d0-a765")).toThrow
      expect(() => UUID.fromString("f81d4fae-7dZc-11d0-a765-00a0c91e6bf6")).toThrow
    }
  }
}
