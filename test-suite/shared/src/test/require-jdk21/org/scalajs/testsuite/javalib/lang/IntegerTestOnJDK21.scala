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

package org.scalajs.testsuite.javalib.lang

import org.junit.Test
import org.junit.Assert._

class IntegerTestOnJDK21 {

  @Test def compress(): Unit = {
    // Example from the doc
    assertEquals(0x000cabab, Integer.compress(0xcafebabe, 0xff00fff0))

    // Random test cases
    assertEquals(0x00000106, Integer.compress(0x89709d4b, 0x7a865060))
    assertEquals(0x000000ab, Integer.compress(0x99933665, 0x0b505400))
    assertEquals(0x00000014, Integer.compress(0x01d4851c, 0x101c1040))
    assertEquals(0x0000020a, Integer.compress(0xd09d94a0, 0x1742a082))
    assertEquals(0x00000032, Integer.compress(0x45ca9572, 0x1ac04203))
    assertEquals(0x00000136, Integer.compress(0xf53bb659, 0x20ee0402))
    assertEquals(0x0000003e, Integer.compress(0x3aca6e68, 0x00304e44))
    assertEquals(0x00000007, Integer.compress(0x80e5df8f, 0x00028500))
    assertEquals(0x0000000e, Integer.compress(0x6ec079f2, 0x10002091))
    assertEquals(0x000007b7, Integer.compress(0xfc8242fc, 0x5e828288))
    assertEquals(0x00000101, Integer.compress(0xccc51aaa, 0xa2184460))
    assertEquals(0x0000001b, Integer.compress(0x8f673f64, 0x04520200))
    assertEquals(0x0000001b, Integer.compress(0xbf6fd8b3, 0x480420a0))
    assertEquals(0x000000ee, Integer.compress(0x3be5a94a, 0x8580e980))
    assertEquals(0x0000001e, Integer.compress(0xb03e6c68, 0x41282810))
    assertEquals(0x00000043, Integer.compress(0x2020edaf, 0x20c30180))
    assertEquals(0x00000001, Integer.compress(0xb385d407, 0x00000001))
    assertEquals(0x00000017, Integer.compress(0x416158fa, 0x12504060))
    assertEquals(0x00000006, Integer.compress(0x857ee772, 0x48806800))
    assertEquals(0x00000057, Integer.compress(0xb7803dee, 0xe001000e))
    assertEquals(0x00000030, Integer.compress(0xb815b083, 0x002c8c44))
    assertEquals(0x00000001, Integer.compress(0x3587593b, 0x00000008))
    assertEquals(0x00000144, Integer.compress(0xb5a433fa, 0xd8004905))
    assertEquals(0x00000007, Integer.compress(0x8b7e53b5, 0x00000031))
    assertEquals(0x0000006a, Integer.compress(0x2f8a5041, 0x12023148))
    assertEquals(0x0000004b, Integer.compress(0x34d1dd9f, 0x620400c9))
    assertEquals(0x00000001, Integer.compress(0x9d6feff4, 0x00800100))
    assertEquals(0x00000000, Integer.compress(0x943fb671, 0x00000000))
    assertEquals(0x00000176, Integer.compress(0x978edc70, 0x044c4232))
    assertEquals(0x00000000, Integer.compress(0x20162f69, 0x00000002))
    assertEquals(0x000000c8, Integer.compress(0x4be28cf0, 0x0040c24e))
    assertEquals(0x00000009, Integer.compress(0xfa162c33, 0x00005a50))
    assertEquals(0x000000f7, Integer.compress(0xc7f24ff6, 0x80905c80))
    assertEquals(0x00000006, Integer.compress(0x2c0da46a, 0x11108003))
    assertEquals(0x00000004, Integer.compress(0x01e9c326, 0x00002681))
    assertEquals(0x00000017, Integer.compress(0xa5978785, 0x00209601))
    assertEquals(0x0000002a, Integer.compress(0xfd14e766, 0x80089003))
    assertEquals(0x00000009, Integer.compress(0xbd1ea1b2, 0x0000c820))
    assertEquals(0x00000002, Integer.compress(0xa07002e3, 0x00002928))
    assertEquals(0x0000000a, Integer.compress(0x81eb15c0, 0x06200841))
    assertEquals(0x0000001e, Integer.compress(0x79d37ad6, 0x02406808))
    assertEquals(0x000001e2, Integer.compress(0xf555014c, 0x110590d0))
    assertEquals(0x0000009a, Integer.compress(0xf7e3e446, 0x02186085))
    assertEquals(0x000000ef, Integer.compress(0xbe25b6b9, 0x94081488))
    assertEquals(0x00000033, Integer.compress(0xc9c80a95, 0x40810490))
    assertEquals(0x0000004c, Integer.compress(0xf8fbd5c8, 0x13208204))
    assertEquals(0x000000b7, Integer.compress(0xba67e36f, 0x08a04528))
    assertEquals(0x0000005d, Integer.compress(0xbd49dddb, 0x00403505))
    assertEquals(0x00000008, Integer.compress(0x40c7f608, 0x00001821))
    assertEquals(0x00000004, Integer.compress(0xc663e6f4, 0x28008102))
  }

  @Test def expand(): Unit = {
    // Example from the doc
    assertEquals(0xca00bab0, Integer.expand(0x000cabab, 0xff00fff0))

    // Random test cases
    assertEquals(0x68804060, Integer.expand(0x89709d4b, 0x7a865060))
    assertEquals(0x03004400, Integer.expand(0x99933665, 0x0b505400))
    assertEquals(0x001c0000, Integer.expand(0x01d4851c, 0x101c1040))
    assertEquals(0x02400000, Integer.expand(0xd09d94a0, 0x1742a082))
    assertEquals(0x12c00002, Integer.expand(0x45ca9572, 0x1ac04203))
    assertEquals(0x004c0002, Integer.expand(0xf53bb659, 0x20ee0402))
    assertEquals(0x00104400, Integer.expand(0x3aca6e68, 0x00304e44))
    assertEquals(0x00028500, Integer.expand(0x80e5df8f, 0x00028500))
    assertEquals(0x10000010, Integer.expand(0x6ec079f2, 0x10002091))
    assertEquals(0x16828200, Integer.expand(0xfc8242fc, 0x5e828288))
    assertEquals(0x20104040, Integer.expand(0xccc51aaa, 0xa2184460))
    assertEquals(0x00100000, Integer.expand(0x8f673f64, 0x04520200))
    assertEquals(0x480000a0, Integer.expand(0xbf6fd8b3, 0x480420a0))
    assertEquals(0x04802100, Integer.expand(0x3be5a94a, 0x8580e980))
    assertEquals(0x41080000, Integer.expand(0xb03e6c68, 0x41282810))
    assertEquals(0x00830180, Integer.expand(0x2020edaf, 0x20c30180))
    assertEquals(0x00000001, Integer.expand(0xb385d407, 0x00000001))
    assertEquals(0x12500040, Integer.expand(0x416158fa, 0x12504060))
    assertEquals(0x48002000, Integer.expand(0x857ee772, 0x48806800))
    assertEquals(0xc001000c, Integer.expand(0xb7803dee, 0xe001000e))
    assertEquals(0x00200044, Integer.expand(0xb815b083, 0x002c8c44))
    assertEquals(0x00000008, Integer.expand(0x3587593b, 0x00000008))
    assertEquals(0xd8004804, Integer.expand(0xb5a433fa, 0xd8004905))
    assertEquals(0x00000021, Integer.expand(0x8b7e53b5, 0x00000031))
    assertEquals(0x02000008, Integer.expand(0x2f8a5041, 0x12023148))
    assertEquals(0x400400c9, Integer.expand(0x34d1dd9f, 0x620400c9))
    assertEquals(0x00000000, Integer.expand(0x9d6feff4, 0x00800100))
    assertEquals(0x00000000, Integer.expand(0x943fb671, 0x00000000))
    assertEquals(0x000c4000, Integer.expand(0x978edc70, 0x044c4232))
    assertEquals(0x00000002, Integer.expand(0x20162f69, 0x00000002))
    assertEquals(0x0040c200, Integer.expand(0x4be28cf0, 0x0040c24e))
    assertEquals(0x00005050, Integer.expand(0xfa162c33, 0x00005a50))
    assertEquals(0x80904c00, Integer.expand(0xc7f24ff6, 0x80905c80))
    assertEquals(0x10100002, Integer.expand(0x2c0da46a, 0x11108003))
    assertEquals(0x00000280, Integer.expand(0x01e9c326, 0x00002681))
    assertEquals(0x00000401, Integer.expand(0xa5978785, 0x00209601))
    assertEquals(0x80001002, Integer.expand(0xfd14e766, 0x80089003))
    assertEquals(0x00000800, Integer.expand(0xbd1ea1b2, 0x0000c820))
    assertEquals(0x00000028, Integer.expand(0xa07002e3, 0x00002928))
    assertEquals(0x00000000, Integer.expand(0x81eb15c0, 0x06200841))
    assertEquals(0x00402800, Integer.expand(0x79d37ad6, 0x02406808))
    assertEquals(0x10041080, Integer.expand(0xf555014c, 0x110590d0))
    assertEquals(0x00100084, Integer.expand(0xf7e3e446, 0x02186085))
    assertEquals(0x84081008, Integer.expand(0xbe25b6b9, 0x94081488))
    assertEquals(0x00800410, Integer.expand(0xc9c80a95, 0x40810490))
    assertEquals(0x10200000, Integer.expand(0xf8fbd5c8, 0x13208204))
    assertEquals(0x00a00528, Integer.expand(0xba67e36f, 0x08a04528))
    assertEquals(0x00401405, Integer.expand(0xbd49dddb, 0x00403505))
    assertEquals(0x00001000, Integer.expand(0x40c7f608, 0x00001821))
    assertEquals(0x20008000, Integer.expand(0xc663e6f4, 0x28008102))
  }

}
