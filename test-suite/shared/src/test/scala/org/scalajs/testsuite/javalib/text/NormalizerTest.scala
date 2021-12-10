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
      normalize("ti\u1ebfng Vi\u1ec7t", NFKD)); // all precomposed

    // Various kinds of spaces
    assertEquals(normalize("Google\u0020Maps", NFKD), // normal space
      normalize("Google\u00a0Maps", NFKD)); // non-breaking space
    assertEquals(normalize("Google\u0020Maps", NFKD), // normal space
      normalize("Google\u2002Maps", NFKD)); // en-space
    assertEquals(normalize("Google\u0020Maps", NFKD), // normal space
      normalize("Google\u2003Maps", NFKD)); // em-space
    assertEquals(normalize("Google\u0020Maps", NFKD), // normal space
      normalize("Google\u3000Maps", NFKC)); // ideographic space

    // Latin small ligature "fi"
    assertEquals(normalize("fi", NFKD), normalize("\ufb01", NFKD));

    // ŀ, Latin small L with middle dot, used in Catalan and often represented
    // as decomposed for non-Unicode environments ( l + ·)
    assertEquals(normalize("l\u00b7", NFKD), normalize("\u0140", NFKD));

    // Legacy text, Japanese narrow Kana (MS-DOS & Win 3.x time)
    assertEquals(normalize("\u30d1\u30bd\u30b3\u30f3", NFKD), // パソコン  :  wide
      normalize("\uff8a\uff9f\uff7f\uff7a\uff9d", NFKD)); // ﾊﾟｿｺﾝ  :  narrow
    // Also for Japanese, Latin fullwidth forms vs. ASCII
    assertEquals(normalize("ABCD", NFKD),
      normalize("\uff21\uff22\uff23\uff24", NFKD)); // ＡＢＣＤ, fullwidth
  }

  

}
