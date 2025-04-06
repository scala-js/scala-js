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

package org.scalajs.linker.backend.wasmemitter

import scala.collection.mutable

import org.scalajs.ir.OriginalName

import org.scalajs.linker.backend.webassembly.Instructions._
import org.scalajs.linker.backend.webassembly.Modules._
import org.scalajs.linker.backend.webassembly.Types._

import EmbeddedConstants._
import VarGen._

private[wasmemitter] final class StringPool {
  import StringPool._

  private val registeredStrings = new mutable.HashSet[String]

  // Set to true by `genPool()`. When true, registering strings is illegal.
  private var poolWasGenerated: Boolean = false

  /** Returns an instruction that loads the given constant string. */
  def getConstantStringInstr(str: String): GlobalGet = {
    if (poolWasGenerated)
      throw new IllegalStateException("The string pool was already generated")

    registeredStrings += str
    GlobalGet(genGlobalID.forStringLiteral(str))
  }

  /** Generates the string pool, and returns the list of WTF-16 strings that
   *  must be provided by the JS embedding.
   */
  def genPool()(implicit ctx: WasmContext): List[(String, String)] = {
    poolWasGenerated = true

    val wtf16Strings = new mutable.ListBuffer[(String, String)]

    for (str <- registeredStrings.toSeq.sorted) {
      if (isValidUTF16String(str)) {
        // Builtin import from the JS string builtins proposal
        ctx.moduleBuilder.addImport(
          Import(
            UTF8StringConstantsModule,
            str, // by definition, the imported member name is the string itself
            ImportDesc.Global(genGlobalID.forStringLiteral(str),
                OriginalName("'" + str), isMutable = false, RefType.extern)
          )
        )
      } else {
        // WTF-16 strings, which we import from a generated JS object
        val indexStr = wtf16Strings.size.toString()
        wtf16Strings += indexStr -> str
        ctx.moduleBuilder.addImport(
          Import(
            WTF16StringConstantsModule,
            indexStr,
            ImportDesc.Global(genGlobalID.forStringLiteral(str),
                OriginalName("wtf16." + indexStr), isMutable = false, RefType.extern)
          )
        )
      }
    }

    wtf16Strings.toList
  }

  private def isValidUTF16String(str: String): Boolean = {
    // scalastyle:off return
    val len = str.length()
    var i = 0
    while (i != len) {
      val cp = str.codePointAt(i)
      // isSurrogate(cp), but Character.isSurrogate only accepts Char's
      if (cp >= Character.MIN_SURROGATE && cp <= Character.MAX_SURROGATE)
        return false
      i += Character.charCount(cp)
    }
    true
    // scalastyle:on return
  }
}

private[wasmemitter] object StringPool {
  private final case class StringData(constantStringIndex: Int, offset: Int)
}
