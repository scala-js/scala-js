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

package org.scalajs.testsuite.javalib.util

import org.junit.Assert._
import org.junit.Test

import java.util.SplittableRandom

import org.scalajs.testsuite.utils.AssertThrows._

class SplittableRandomTest {

  @Test def should_correctly_implement_nextLong(): Unit = {
    val sr1 = new SplittableRandom(205620432625028L)
    assertEquals(-546649510716590878L, sr1.nextLong())
    assertEquals(5574037117696891406L, sr1.nextLong())
    assertEquals(-2877648745898596966L, sr1.nextLong())
    assertEquals(5734720902145206190L, sr1.nextLong())
    assertEquals(1684781725002208217L, sr1.nextLong())
    assertEquals(687902890032948154L, sr1.nextLong())
    assertEquals(176280366443457561L, sr1.nextLong())
    assertEquals(-2944062288620903198L, sr1.nextLong())
    assertEquals(6872063775710978746L, sr1.nextLong())
    assertEquals(-7374959378916621341L, sr1.nextLong())

    val sr2 = new SplittableRandom(-7374959378916621341L)
    assertEquals(3241340805431811560L, sr2.nextLong())
    assertEquals(-2124831722811234979L, sr2.nextLong())
    assertEquals(7339249063279462363L, sr2.nextLong())
    assertEquals(1969867631102365324L, sr2.nextLong())
    assertEquals(81632902222022867L, sr2.nextLong())
    assertEquals(3451014011249622471L, sr2.nextLong())
    assertEquals(-1727223780574897556L, sr2.nextLong())
    assertEquals(-5128686556801302975L, sr2.nextLong())
    assertEquals(-6412221907719417908L, sr2.nextLong())
    assertEquals(-110482401893286265L, sr2.nextLong())
  }

  @Test def should_correctly_implement_nextInt(): Unit = {
    val sr1 = new SplittableRandom(-84638)
    assertEquals(962946964, sr1.nextInt())
    assertEquals(1723227640, sr1.nextInt())
    assertEquals(-621790539, sr1.nextInt())
    assertEquals(-1848500421, sr1.nextInt())
    assertEquals(-614898617, sr1.nextInt())
    assertEquals(-628601850, sr1.nextInt())
    assertEquals(-463597391, sr1.nextInt())
    assertEquals(1874082924, sr1.nextInt())
    assertEquals(-1206032701, sr1.nextInt())
    assertEquals(1549874426, sr1.nextInt())

    val sr2 = new SplittableRandom(1549874426)
    assertEquals(-495782737, sr2.nextInt())
    assertEquals(-1487672352, sr2.nextInt())
    assertEquals(-538628223, sr2.nextInt())
    assertEquals(1117712970, sr2.nextInt())
    assertEquals(2081437683, sr2.nextInt())
    assertEquals(2134440938, sr2.nextInt())
    assertEquals(-2102672277, sr2.nextInt())
    assertEquals(832521577, sr2.nextInt())
    assertEquals(518494223, sr2.nextInt())
    assertEquals(-42114979, sr2.nextInt())
  }

  @Test def should_correctly_implement_nextDouble(): Unit = {
    val sr1 = new SplittableRandom(-45)
    assertEquals(0.8229662358649753, sr1.nextDouble(), 0.0)
    assertEquals(0.43324117570991283, sr1.nextDouble(), 0.0)
    assertEquals(0.2639712712295723, sr1.nextDouble(), 0.0)
    assertEquals(0.5576376282289696, sr1.nextDouble(), 0.0)
    assertEquals(0.5505810186639037, sr1.nextDouble(), 0.0)
    assertEquals(0.3944509738261206, sr1.nextDouble(), 0.0)
    assertEquals(0.3108138671457821, sr1.nextDouble(), 0.0)
    assertEquals(0.585951421265481, sr1.nextDouble(), 0.0)
    assertEquals(0.2009547438834305, sr1.nextDouble(), 0.0)
    assertEquals(0.8317691736686829, sr1.nextDouble(), 0.0)

    val sr2 = new SplittableRandom(45)
    assertEquals(0.9684135896502549, sr2.nextDouble(), 0.0)
    assertEquals(0.9819686323309464, sr2.nextDouble(), 0.0)
    assertEquals(0.5311927268453047, sr2.nextDouble(), 0.0)
    assertEquals(0.8521356026917833, sr2.nextDouble(), 0.0)
    assertEquals(0.01880601374789126, sr2.nextDouble(), 0.0)
    assertEquals(0.37792881248018584, sr2.nextDouble(), 0.0)
    assertEquals(0.7179744490511354, sr2.nextDouble(), 0.0)
    assertEquals(0.3448879713662756, sr2.nextDouble(), 0.0)
    assertEquals(0.023020123407108684, sr2.nextDouble(), 0.0)
    assertEquals(0.6454709437764473, sr2.nextDouble(), 0.0)
  }

  @Test def should_correctly_implement_nextBoolean(): Unit = {
    val sr1 = new SplittableRandom(4782934)
    assertFalse(sr1.nextBoolean())
    assertFalse(sr1.nextBoolean())
    assertTrue(sr1.nextBoolean())
    assertTrue(sr1.nextBoolean())
    assertTrue(sr1.nextBoolean())
    assertFalse(sr1.nextBoolean())
    assertFalse(sr1.nextBoolean())
    assertTrue(sr1.nextBoolean())
    assertTrue(sr1.nextBoolean())
    assertTrue(sr1.nextBoolean())

    val sr2 = new SplittableRandom(-4782934)
    assertFalse(sr2.nextBoolean())
    assertFalse(sr2.nextBoolean())
    assertTrue(sr2.nextBoolean())
    assertTrue(sr2.nextBoolean())
    assertTrue(sr2.nextBoolean())
    assertFalse(sr2.nextBoolean())
    assertFalse(sr2.nextBoolean())
    assertTrue(sr2.nextBoolean())
    assertTrue(sr2.nextBoolean())
    assertTrue(sr2.nextBoolean())
  }

  @Test def should_correctly_implement_split(): Unit = {
    val sr1 = new SplittableRandom(205620432625028L).split()
    assertEquals(-2051870635339219700L, sr1.nextLong())
    assertEquals(-4512002368431042276L, sr1.nextLong())

    val sr2 = new SplittableRandom(-4512002368431042276L).split()
    assertEquals(7607532382842316154L, sr2.nextLong())
    assertEquals(-1011899051174066375L, sr2.nextLong())

    val sr3 = new SplittableRandom(7607532382842316154L).split()
    assertEquals(-1531465968943756660L, sr3.nextLong())
    assertEquals(948449286892387518L, sr3.nextLong())

    val sr4 = new SplittableRandom(948449286892387518L).split()
    assertEquals(2486448889230464769L, sr4.nextLong())
    assertEquals(4550542803092639410L, sr4.nextLong())

    val sr5 = sr4.split()
    assertEquals(8668601242423591169L, sr5.nextLong())
    assertEquals(-986244092642826172L, sr5.nextLong())

    val sr6 = sr4.split()
    assertEquals(274792684182118046L, sr6.nextLong())
    assertEquals(683259797650761389L, sr6.nextLong())

    val sr7 = sr6.split()
    assertEquals(1682793527903105269L, sr7.nextLong())
    assertEquals(2140483520539013019L, sr7.nextLong())

    val sr8 = sr6.split()
    assertEquals(-7468768144124082123L, sr8.nextLong())
    assertEquals(6163667569279435512L, sr8.nextLong())
  }

}
