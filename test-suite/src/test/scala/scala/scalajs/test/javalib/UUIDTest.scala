package scala.scalajs.test

import java.util.UUID

object UUIDTest extends JasmineTest {

  describe("java.util.UUID") {
    
    it("should construct an UUID from a string") {
      val uuid = UUID.fromString("59cd622a-3cd5-45c2-b5de-b0018e4cd1bd")
      expect(uuid.getLeastSignificantBits).toBe(-5341638587285974595L)
      expect(uuid.getMostSignificantBits).toBe(6470936173150029250L)

      val uuid2 = UUID.fromString("59CD622A-3CD5-45C2-B5DE-B0018E4CD1BD")
      expect(uuid2.getLeastSignificantBits).toBe(-5341638587285974595L)
      expect(uuid2.getMostSignificantBits).toBe(6470936173150029250L)

      expect(uuid == uuid2).toBe(true)
    }

    it("should throw an exception when constructed from a wrongly formatted string") {
      expect(() => UUID.fromString("59cd622a-3cd5-45c2-b5de-b0018e4cd")).toThrow
      expect(() => UUID.fromString("the answer to life the universe and everything")).toThrow
    }

    it("should be formatted as a string") {
      val uuid = new UUID(42, 42)
      val uuid2 = new UUID(-7747894900819184095L, -3678886078540396238L)
      expect(uuid.toString).toBe("00000000-0000-002a-0000-00000000002a")
      expect(uuid2.toString).toBe("9479f2a3-479c-b621-ccf1-f89a67784932")
    }

    it("should provide `compareTo`") {
      val uuid = new UUID(42, 42)
      val uuid2 = new UUID(-7747894900819184095L, -3678886078540396238L)
      expect(uuid.compareTo(uuid2)).toBeGreaterThan(0)
      expect(uuid2.compareTo(uuid)).toBeLessThan(0)
      expect(uuid.compareTo(uuid)).toEqual(0)
    }

    it("should be a Comparable") {
      val uuid = new UUID(42, 42)
      val uuid2 = new UUID(-7747894900819184095L, -3678886078540396238L)
      expect(uuid.asInstanceOf[Comparable[Any]].compareTo(uuid2)).toBeGreaterThan(0)
      expect(uuid2.asInstanceOf[Comparable[Any]].compareTo(uuid)).toBeLessThan(0)
      expect(uuid.asInstanceOf[Comparable[Any]].compareTo(uuid)).toEqual(0)
    }

    it("should provide a correct version") {
      val uuid = UUID.fromString("0b13c72d-f0e5-4d6a-a0e6-5c5cbf864d4d")
      val uuid2 = UUID.randomUUID
      expect(uuid.version).toEqual(4)
      expect(uuid2.version).toEqual(4)
    }

    it("should provide a correct variant") {
      val uuid = UUID.fromString("0b13c72d-f0e5-4d6a-a0e6-5c5cbf864d4d")
      val uuid2 = UUID.randomUUID
      expect(uuid.variant).toEqual(2) // IETF variant
      expect(uuid2.variant).toEqual(2) // IETF variant
    }

    it("should provide a correct timestamp") {
      val uuid = new UUID(4242, 4242)
      expect(uuid.timestamp).toEqual(41095346599755776L)
    }

    it("should provide a correct clockSequence") {
      val uuid = new UUID(4242, 4242)
      expect(uuid.clockSequence).toEqual(0)
 
      // version has been set to 1 (required for time based uuid)
      val uuid3 = UUID.fromString("dac079bd-4740-132c-dde1-1f64b93635d9") 
      expect(uuid3.clockSequence).toBe(7649)

      // randomUUID set version to 4
      val uuid2 = UUID.randomUUID
      expect(() => uuid2.clockSequence).toThrow
    }

    it("should provide a correct node") {
      val uuid = new UUID(4242, 4242)
      expect(uuid.node).toEqual(4242)

      // version has been set to 1 (required for time based uuid)
      val uuid2 = UUID.fromString("dac079bd-4740-132c-dde1-1f64b93635d9") 
      expect(uuid2.node).toBe(34517464528345L)
    }

    it("should provide a correct hashCode") {
      val uuid = UUID.fromString("0b13c72d-f0e5-4d6a-a0e6-5c5cbf864d4d")
      expect(uuid.hashCode).toEqual(-459891882)
    }

    it("should provide equals") {
      val uuid = UUID.fromString("0b13c72d-f0e5-4d6a-a0e6-5c5cbf864d4d")
      val uuid2 = new UUID(798200559103724906L, -6852688229577634483L)
      val uuid3 = UUID.fromString("dac079bd-4740-132c-dde1-1f64b93635d9") 
      expect(uuid == uuid2).toEqual(true)
      expect(uuid == uuid3).toEqual(false)
    }
  }

}
