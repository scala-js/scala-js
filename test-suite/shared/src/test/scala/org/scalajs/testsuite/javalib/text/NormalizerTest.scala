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

// Copyright 2013 the V8 project authors. All rights reserved.
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above
//       copyright notice, this list of conditions and the following
//       disclaimer in the documentation and/or other materials provided
//       with the distribution.
//     * Neither the name of Google Inc. nor the names of its
//       contributors may be used to endorse or promote products derived
//       from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

package org.scalajs.testsuite.javalib.text

import java.text.Normalizer
import java.text.Normalizer.Form

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.AssertThrows.assertThrows

/**
 * Adapted from https://github.com/v8/v8/blob/lkgr/test/intl/string/normalization.js
 */
class NormalizerTest {
  
  import Normalizer.normalize
  import Form._

  // Common use case when searching for "not very exact" match.
  // These are examples of data one might encounter in real use.
  @Test def realUseCases(): Unit = {
    // Vietnamese legacy text, old Windows 9x / non-Unicode applications use
    // windows-1258 code page, which is neither precomposed, nor decomposed.
    assertEquals(normalize("ti\u00ea\u0301ng Vi\u00ea\u0323t", NFKD),
      normalize("ti\u1ebfng Vi\u1ec7t", NFKD)) // all precomposed

    // Various kinds of spaces
    assertEquals(normalize("Google\u0020Maps", NFKD), // normal space
      normalize("Google\u00a0Maps", NFKD)) // non-breaking space
    assertEquals(normalize("Google\u0020Maps", NFKD), // normal space
      normalize("Google\u2002Maps", NFKD)) // en-space
    assertEquals(normalize("Google\u0020Maps", NFKD), // normal space
      normalize("Google\u2003Maps", NFKD)) // em-space
    assertEquals(normalize("Google\u0020Maps", NFKD), // normal space
      normalize("Google\u3000Maps", NFKC)) // ideographic space

    // Latin small ligature "fi"
    assertEquals(normalize("fi", NFKD), normalize("\ufb01", NFKD))

    // ŀ, Latin small L with middle dot, used in Catalan and often represented
    // as decomposed for non-Unicode environments ( l + ·)
    assertEquals(normalize("l\u00b7", NFKD), normalize("\u0140", NFKD))

    // Legacy text, Japanese narrow Kana (MS-DOS & Win 3.x time)
    assertEquals(normalize("\u30d1\u30bd\u30b3\u30f3", NFKD), // パソコン  :  wide
      normalize("\uff8a\uff9f\uff7f\uff7a\uff9d", NFKD)) // ﾊﾟｿｺﾝ  :  narrow
    // Also for Japanese, Latin fullwidth forms vs. ASCII
    assertEquals(normalize("ABCD", NFKD),
      normalize("\uff21\uff22\uff23\uff24", NFKD)) // ＡＢＣＤ, fullwidth
  }

  @Test def edgeCases(): Unit = {
    assertThrows(classOf[NullPointerException], normalize("ABC", null))
    assertThrows(classOf[NullPointerException], normalize(null, NFKD))
    assertThrows(classOf[NullPointerException], normalize(null, null))
  }

  // Several kinds of mappings. No need to be comprehensive, we don't test
  // the ICU functionality, we only test C - JavaScript 'glue'
  val testData = Array(
    // org, default, NFC, NFD, NKFC, NKFD
    Array("\u00c7", // Ç : Combining sequence, Latin 1
      "\u00c7", "\u0043\u0327",
      "\u00c7", "\u0043\u0327"),
    Array("\u0218", // Ș : Combining sequence, non-Latin 1
      "\u0218", "\u0053\u0326",
      "\u0218", "\u0053\u0326"),
    Array("\uac00", // 가 : Hangul
      "\uac00", "\u1100\u1161",
      "\uac00", "\u1100\u1161"),
    Array("\uff76", // ｶ : Narrow Kana
      "\uff76", "\uff76",
      "\u30ab", "\u30ab"),
    Array("\u00bc", // ¼ : Fractions
      "\u00bc", "\u00bc",
      "\u0031\u2044\u0034", "\u0031\u2044\u0034"),
    Array("\u01c6", // ǆ  : Latin ligature
      "\u01c6", "\u01c6",
      "\u0064\u017e", "\u0064\u007a\u030c"),
    Array("s\u0307\u0323", // s + dot above + dot below, ordering of combining marks
      "\u1e69", "s\u0323\u0307",
      "\u1e69", "s\u0323\u0307"),
    Array("\u3300", // ㌀ : Squared characters
      "\u3300", "\u3300",
      "\u30a2\u30d1\u30fc\u30c8", // アパート
      "\u30a2\u30cf\u309a\u30fc\u30c8"), // アパート
    Array("\ufe37", // ︷ : Vertical forms
      "\ufe37", "\ufe37",
      "{" , "{"),
    Array("\u2079", // ⁹ : superscript 9
      "\u2079", "\u2079",
      "9", "9"),
    Array("\ufee5\ufee6\ufee7\ufee8", // Arabic forms
      "\ufee5\ufee6\ufee7\ufee8", "\ufee5\ufee6\ufee7\ufee8",
      "\u0646\u0646\u0646\u0646", "\u0646\u0646\u0646\u0646"),
    Array("\u2460", // ① : Circled
      "\u2460", "\u2460",
      "1", "1"),
    Array("\u210c", // ℌ : Font variants
      "\u210c", "\u210c",
      "H", "H"),
    Array("\u2126", // Ω : Singleton, OHM sign vs. Greek capital letter OMEGA
      "\u03a9", "\u03a9",
      "\u03a9", "\u03a9"),
    Array("\ufdfb", // Long ligature, ARABIC LIGATURE JALLAJALALOUHOU
      "\ufdfb", "\ufdfb",
      "\u062C\u0644\u0020\u062C\u0644\u0627\u0644\u0647",
      "\u062C\u0644\u0020\u062C\u0644\u0627\u0644\u0647")
  )

  @Test def array(): Unit = {
    val kNFC = 1
    val kNFD = 2
    val kNFKC = 3
    val kNFKD = 4
    for (i <- testData.indices) {
      // the original, NFC and NFD should normalize to the same thing
      for (column <- 0 until 4) {
        var str = testData(i)(column)
        assertEquals(normalize(str, NFC), testData(i)(kNFC))
        assertEquals(normalize(str, NFD), testData(i)(kNFD))
        assertEquals(normalize(str, NFKC), testData(i)(kNFKC))
        assertEquals(normalize(str, NFKD), testData(i)(kNFKD))
      }
    }
  }

}
