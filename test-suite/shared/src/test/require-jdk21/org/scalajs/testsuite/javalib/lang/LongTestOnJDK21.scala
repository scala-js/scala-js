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

import java.lang.{Long => JLong}

import org.junit.Test
import org.junit.Assert._

class LongTestOnJDK21 {

  @Test def compress(): Unit = {
    // Example from the doc
    assertEquals(0x00000000000cababL, JLong.compress(0x00000000cafebabeL, 0x00000000ff00fff0L))

    // Random test cases
    assertEquals(0x0000000000000000L, JLong.compress(0x8ba5e11cb2dbe50bL, 0x0000000000000000L))
    assertEquals(0x0000000000000c41L, JLong.compress(0xe40e22f388afccf8L, 0x026c1c0224502020L))
    assertEquals(0x00000000000013b4L, JLong.compress(0xeeae0b35b17b8194L, 0x02110024291400e0L))
    assertEquals(0x0000000000007a15L, JLong.compress(0x345ae610c64f3d4eL, 0x834a336481020012L))
    assertEquals(0x0000000000000b1aL, JLong.compress(0x7641d455e2ed281bL, 0x008902403e041880L))
    assertEquals(0x000000000000004eL, JLong.compress(0xac861cf1b05b0398L, 0x0000000000101a8cL))
    assertEquals(0x0000000000002a9aL, JLong.compress(0xc06624be2e92bf17L, 0x0000950905c41138L))
    assertEquals(0x000000000001c3b5L, JLong.compress(0xdbb3627dc7fbd3aeL, 0x103418a80026ba00L))
    assertEquals(0x00000000000cdd88L, JLong.compress(0xa882378c6c18041bL, 0x2d82919c00803704L))
    assertEquals(0x0000000000000000L, JLong.compress(0x6f38dc8510006c48L, 0x0000000000000000L))
    assertEquals(0x000000000000066bL, JLong.compress(0x06b883c2e379e9efL, 0x00000008c8b23003L))
    assertEquals(0x00000000000007c3L, JLong.compress(0x91f7e88ba084ce40L, 0x4114d00001128200L))
    assertEquals(0x0000000000002226L, JLong.compress(0x02c045d59624182dL, 0x006304006045008eL))
    assertEquals(0x0000000000000001L, JLong.compress(0xbf4d46a3bafce253L, 0x0000000000000010L))
    assertEquals(0x000000000000001bL, JLong.compress(0x33237efb1e633f63L, 0x000000000000884bL))
    assertEquals(0x0000000000000001L, JLong.compress(0x93107f2bacf81b2fL, 0x0000000000000008L))
    assertEquals(0x0000000000003363L, JLong.compress(0xc2bb29b362f8e219L, 0x08520262c02400b8L))
    assertEquals(0x000000000000b877L, JLong.compress(0xc78b262b39eb2cddL, 0xa68050843800880cL))
    assertEquals(0x00000000000028ceL, JLong.compress(0x7d930cac7d303e64L, 0x00398010c4053810L))
    assertEquals(0x0000000000000057L, JLong.compress(0x08ee90c1822e6c4aL, 0x000004108a1c0008L))
    assertEquals(0x000000000006885cL, JLong.compress(0xde0c2c816052fc18L, 0x062582003636040eL))
    assertEquals(0x0000000000031203L, JLong.compress(0xaa1ba08c10d8c985L, 0x0810450901a1226dL))
    assertEquals(0x00000000000001c0L, JLong.compress(0x440b3f6882e34c60L, 0x0a9a040041000107L))
    assertEquals(0x000000000000003bL, JLong.compress(0x68a311b59b1ab404L, 0x000000040800e004L))
    assertEquals(0x00000000000357caL, JLong.compress(0xfa906cbb8fd930deL, 0x20d138200a445403L))
    assertEquals(0x000000000000026cL, JLong.compress(0x38d1ba9226fa0ba6L, 0x2610d00030000040L))
    assertEquals(0x00000000000000fbL, JLong.compress(0xf36f2165fba941c7L, 0x1102000020800016L))
    assertEquals(0x0000000000001a0aL, JLong.compress(0x00c5ef0a8c060754L, 0x0604201802509192L))
    assertEquals(0x0000000000003a32L, JLong.compress(0x830c388d7aa0f394L, 0x00011801a00a074dL))
    assertEquals(0x000000000000000fL, JLong.compress(0x70fbac9a03d7ba53L, 0x00000000000e0210L))
    assertEquals(0x000000000003f9ebL, JLong.compress(0x0eb1be8df6d84d3aL, 0x6c91912806005170L))
    assertEquals(0x0000000000000009L, JLong.compress(0x0f352e179f551dc5L, 0x0000000000018024L))
    assertEquals(0x0000000000005072L, JLong.compress(0x5552eede68f76decL, 0x7a2c800c02008006L))
    assertEquals(0x000000000000bb68L, JLong.compress(0x042fbb7df8e7a486L, 0x5018420f0a034541L))
    assertEquals(0x0000000000000e61L, JLong.compress(0x063102eb1ee90360L, 0x0000002a6884e020L))
    assertEquals(0x00000000000010fcL, JLong.compress(0x48e2a60f7acfb970L, 0x1a14420e12200004L))
    assertEquals(0x000000000000000aL, JLong.compress(0x34c3b8688f7469aaL, 0x0000000000005840L))
    assertEquals(0x00000000000552caL, JLong.compress(0x22f86b59622f04b9L, 0x0021452aa043a584L))
    assertEquals(0x0000000000000000L, JLong.compress(0xa83647a4aa67df00L, 0x0000000000000065L))
    assertEquals(0x0000000000000137L, JLong.compress(0x744857857573feb5L, 0x000001008c262800L))
    assertEquals(0x0000000000000005L, JLong.compress(0xf42e4dbd7d7143ccL, 0x0000000000018004L))
    assertEquals(0x0000000000000a8eL, JLong.compress(0x4b2fb8215e743fb8L, 0x0542020040889421L))
    assertEquals(0x0000000000060a4cL, JLong.compress(0xa71c17c938f8a501L, 0xa0c18a100d23a090L))
    assertEquals(0x0000000000000000L, JLong.compress(0x7a405827c85af885L, 0x0000000000810010L))
    assertEquals(0x000000000009af25L, JLong.compress(0xaea0f896b0fa389bL, 0x2003c2a6ac100235L))
    assertEquals(0x0000000000000004L, JLong.compress(0x55b341855d14f960L, 0x000000000000002cL))
    assertEquals(0x00000000000001b3L, JLong.compress(0x6cfd8fa32717da97L, 0x000000042230a111L))
    assertEquals(0x000000000005616aL, JLong.compress(0x0ea774c43535bee0L, 0x480a940b81a45188L))
    assertEquals(0x0000000000000315L, JLong.compress(0x73d1d6786823626bL, 0x040209064a4a12c0L))
    assertEquals(0x00000000000000fdL, JLong.compress(0xd918a1dda6d9f1e4L, 0x0000000006185300L))
  }

  @Test def expand(): Unit = {
    // Example from the doc
    assertEquals(0x00000000ca00bab0L, JLong.expand(0x00000000000cababL, 0x00000000ff00fff0L))

    // Random test cases
    assertEquals(0x0000000000000000L, JLong.expand(0x8ba5e11cb2dbe50bL, 0x0000000000000000L))
    assertEquals(0x020c040224400000L, JLong.expand(0xe40e22f388afccf8L, 0x026c1c0224502020L))
    assertEquals(0x0000000420100080L, JLong.expand(0xeeae0b35b17b8194L, 0x02110024291400e0L))
    assertEquals(0x820a312001020010L, JLong.expand(0x345ae610c64f3d4eL, 0x834a336481020012L))
    assertEquals(0x0081000002040880L, JLong.expand(0x7641d455e2ed281bL, 0x008902403e041880L))
    assertEquals(0x0000000000000a00L, JLong.expand(0xac861cf1b05b0398L, 0x0000000000101a8cL))
    assertEquals(0x0000850905001038L, JLong.expand(0xc06624be2e92bf17L, 0x0000950905c41138L))
    assertEquals(0x1030102800223800L, JLong.expand(0xdbb3627dc7fbd3aeL, 0x103418a80026ba00L))
    assertEquals(0x2000008000001504L, JLong.expand(0xa882378c6c18041bL, 0x2d82919c00803704L))
    assertEquals(0x0000000000000000L, JLong.expand(0x6f38dc8510006c48L, 0x0000000000000000L))
    assertEquals(0x0000000808b03003L, JLong.expand(0x06b883c2e379e9efL, 0x00000008c8b23003L))
    assertEquals(0x4110400000000000L, JLong.expand(0x91f7e88ba084ce40L, 0x4114d00001128200L))
    assertEquals(0x002200000004008aL, JLong.expand(0x02c045d59624182dL, 0x006304006045008eL))
    assertEquals(0x0000000000000010L, JLong.expand(0xbf4d46a3bafce253L, 0x0000000000000010L))
    assertEquals(0x0000000000008003L, JLong.expand(0x33237efb1e633f63L, 0x000000000000884bL))
    assertEquals(0x0000000000000008L, JLong.expand(0x93107f2bacf81b2fL, 0x0000000000000008L))
    assertEquals(0x0850002000040088L, JLong.expand(0xc2bb29b362f8e219L, 0x08520262c02400b8L))
    assertEquals(0x0480400428008804L, JLong.expand(0xc78b262b39eb2cddL, 0xa68050843800880cL))
    assertEquals(0x0019801004041000L, JLong.expand(0x7d930cac7d303e64L, 0x00398010c4053810L))
    assertEquals(0x0000000080140000L, JLong.expand(0x08ee90c1822e6c4aL, 0x000004108a1c0008L))
    assertEquals(0x0205820030020400L, JLong.expand(0xde0c2c816052fc18L, 0x062582003636040eL))
    assertEquals(0x0000440100210009L, JLong.expand(0xaa1ba08c10d8c985L, 0x0810450901a1226dL))
    assertEquals(0x0280040040000000L, JLong.expand(0x440b3f6882e34c60L, 0x0a9a040041000107L))
    assertEquals(0x0000000000004000L, JLong.expand(0x68a311b59b1ab404L, 0x000000040800e004L))
    assertEquals(0x0081200002405402L, JLong.expand(0xfa906cbb8fd930deL, 0x20d138200a445403L))
    assertEquals(0x2600800030000000L, JLong.expand(0x38d1ba9226fa0ba6L, 0x2610d00030000040L))
    assertEquals(0x1100000000000016L, JLong.expand(0xf36f2165fba941c7L, 0x1102000020800016L))
    assertEquals(0x0000001802101080L, JLong.expand(0x00c5ef0a8c060754L, 0x0604201802509192L))
    assertEquals(0x00011800200a0108L, JLong.expand(0x830c388d7aa0f394L, 0x00011801a00a074dL))
    assertEquals(0x0000000000080210L, JLong.expand(0x70fbac9a03d7ba53L, 0x00000000000e0210L))
    assertEquals(0x4010110800005120L, JLong.expand(0x0eb1be8df6d84d3aL, 0x6c91912806005170L))
    assertEquals(0x0000000000008004L, JLong.expand(0x0f352e179f551dc5L, 0x0000000000018024L))
    assertEquals(0x6a0c800802008000L, JLong.expand(0x5552eede68f76decL, 0x7a2c800c02008006L))
    assertEquals(0x5018020202000140L, JLong.expand(0x042fbb7df8e7a486L, 0x5018420f0a034541L))
    assertEquals(0x0000000248800000L, JLong.expand(0x063102eb1ee90360L, 0x0000002a6884e020L))
    assertEquals(0x1a00400e00000000L, JLong.expand(0x48e2a60f7acfb970L, 0x1a14420e12200004L))
    assertEquals(0x0000000000004800L, JLong.expand(0x34c3b8688f7469aaL, 0x0000000000005840L))
    assertEquals(0x002140008002a404L, JLong.expand(0x22f86b59622f04b9L, 0x0021452aa043a584L))
    assertEquals(0x0000000000000000L, JLong.expand(0xa83647a4aa67df00L, 0x0000000000000065L))
    assertEquals(0x0000000084220800L, JLong.expand(0x744857857573feb5L, 0x000001008c262800L))
    assertEquals(0x0000000000010000L, JLong.expand(0xf42e4dbd7d7143ccL, 0x0000000000018004L))
    assertEquals(0x0542020040089000L, JLong.expand(0x4b2fb8215e743fb8L, 0x0542020040889421L))
    assertEquals(0x0040801004000010L, JLong.expand(0xa71c17c938f8a501L, 0xa0c18a100d23a090L))
    assertEquals(0x0000000000800010L, JLong.expand(0x7a405827c85af885L, 0x0000000000810010L))
    assertEquals(0x200100a408000225L, JLong.expand(0xaea0f896b0fa389bL, 0x2003c2a6ac100235L))
    assertEquals(0x0000000000000000L, JLong.expand(0x55b341855d14f960L, 0x000000000000002cL))
    assertEquals(0x0000000402008111L, JLong.expand(0x6cfd8fa32717da97L, 0x000000042230a111L))
    assertEquals(0x0802840b80a40000L, JLong.expand(0x0ea774c43535bee0L, 0x480a940b81a45188L))
    assertEquals(0x00020800404810c0L, JLong.expand(0x73d1d6786823626bL, 0x040209064a4a12c0L))
    assertEquals(0x0000000006101000L, JLong.expand(0xd918a1dda6d9f1e4L, 0x0000000006185300L))
  }

}
