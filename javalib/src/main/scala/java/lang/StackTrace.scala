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

package java.lang

import scala.annotation.tailrec

import scala.scalajs.js
import scala.scalajs.js.JSStringOps.enableJSStringOps

import Utils._

/** Conversions of JavaScript stack traces to Java stack traces.
 */
private[lang] object StackTrace {

  /* !!! Note that in this unit, we go to great lengths *not* to use anything
   * from the Scala collections library.
   *
   * This minimizes the risk of runtime errors during the process of decoding
   * errors, which would be very bad if it happened.
   */

  /** Returns the current stack trace.
   *  If the stack trace cannot be analyzed in meaningful way (because we don't
   *  know the browser), an empty array is returned.
   */
  def getCurrentStackTrace(): Array[StackTraceElement] =
    extract(createException().asInstanceOf[js.Dynamic])

  /** Captures browser-specific state recording the current stack trace.
   *  The state is stored as a magic field of the throwable, and will be used
   *  by `extract()` to create an Array[StackTraceElement].
   */
  @inline def captureState(throwable: Throwable): Unit = {
    val throwableAsJSAny = throwable.asInstanceOf[js.Any]
    val identifyingString: Any = {
      js.constructorOf[js.Object].prototype
        .selectDynamic("toString")
        .call(throwableAsJSAny)
    }
    if ("[object Error]" == identifyingString) {
      /* The `throwable` has an `[[ErrorData]]` internal slot, which is as good
       * a guarantee as any that it contains stack trace data itself. In
       * practice, this happens when we emit ES 2015 classes, and no other
       * compiler down the line has compiled them away as ES 5.1 functions and
       * prototypes.
       */
      captureState(throwable, throwable)
    } else if (js.constructorOf[js.Error].captureStackTrace eq ().asInstanceOf[AnyRef]) {
      captureState(throwable, createException())
    } else {
      /* V8-specific.
       * The Error.captureStackTrace(e) method records the current stack trace
       * on `e` as would do `new Error()`, thereby turning `e` into a proper
       * exception. This avoids creating a dummy exception, but is mostly
       * important so that Node.js will show stack traces if the exception
       * is never caught and reaches the global event queue.
       */
      js.constructorOf[js.Error].captureStackTrace(throwableAsJSAny)
      captureState(throwable, throwable)
    }
  }

  /** Creates a JS Error with the current stack trace state. */
  @inline private def createException(): Any =
    new js.Error()

  /** Captures browser-specific state recording the stack trace of a JS error.
   *  The state is stored as a magic field of the throwable, and will be used
   *  by `extract()` to create an Array[StackTraceElement].
   */
  @inline def captureState(throwable: Throwable, e: Any): Unit =
    throwable.setStackTraceStateInternal(e)

  /** Extracts a throwable's stack trace from captured browser-specific state.
   *  If no stack trace state has been recorded, or if the state cannot be
   *  analyzed in meaningful way (because we don't know the browser), an
   *  empty array is returned.
   */
  def extract(throwable: Throwable): Array[StackTraceElement] =
    extract(throwable.getStackTraceStateInternal())

  /** Extracts a stack trace from captured browser-specific stackdata.
   *  If no stack trace state has been recorded, or if the state cannot be
   *  analyzed in meaningful way (because we don't know the browser), an
   *  empty array is returned.
   */
  private def extract(stackdata: Any): Array[StackTraceElement] = {
    val lines = normalizeStackTraceLines(stackdata.asInstanceOf[js.Dynamic])
    normalizedLinesToStackTrace(lines)
  }

  /* Converts an array of frame entries in normalized form to a stack trace.
   * Each line must have either the format
   *   <functionName>@<fileName>:<lineNumber>:<columnNumber>
   * or
   *   <functionName>@<fileName>:<lineNumber>
   * For some reason, on some browsers, we sometimes have empty lines too.
   * In the rest of the function, we convert the non-empty lines into
   * StackTraceElements.
   */
  private def normalizedLinesToStackTrace(
      lines: js.Array[String]): Array[StackTraceElement] = {

    val NormalizedFrameLine = """^([^\@]*)\@(.*):([0-9]+)$""".re
    val NormalizedFrameLineWithColumn = """^([^\@]*)\@(.*):([0-9]+):([0-9]+)$""".re

    @inline def parseInt(s: String): Int =
      js.Dynamic.global.parseInt(s).asInstanceOf[Int]

    val trace = js.Array[StackTraceElement]()
    var i = 0
    while (i < lines.length) {
      val line = lines(i)
      if (!line.isEmpty) {
        val mtch1 = NormalizedFrameLineWithColumn.exec(line)
        if (mtch1 ne null) {
          val classAndMethodName =
            extractClassMethod(undefOrForceGet(mtch1(1)))
          val elem = new StackTraceElement(classAndMethodName(0),
              classAndMethodName(1), undefOrForceGet(mtch1(2)),
              parseInt(undefOrForceGet(mtch1(3))))
          elem.setColumnNumber(parseInt(undefOrForceGet(mtch1(4))))
          trace.push(elem)
        } else {
          val mtch2 = NormalizedFrameLine.exec(line)
          if (mtch2 ne null) {
            val classAndMethodName =
              extractClassMethod(undefOrForceGet(mtch2(1)))
            trace.push(new StackTraceElement(classAndMethodName(0),
                classAndMethodName(1), undefOrForceGet(mtch2(2)),
                parseInt(undefOrForceGet(mtch2(3)))))
          } else {
            // just in case
            trace.push(new StackTraceElement("<jscode>", line, null, -1))
          }
        }
      }
      i += 1
    }

    // Convert the JS array into a Scala array
    val len = trace.length
    val result = new Array[StackTraceElement](len)
    i = 0
    while (i < len) {
      result(i) = trace(i)
      i += 1
    }

    result
  }

  /** Tries and extract the class name and method from the JS function name.
   *
   *  The recognized patterns are
   *  {{{
   *    new \$c_<encoded class name>
   *    \$c_<encoded class name>.prototype.<encoded method name>
   *    \$b_<encoded class name>.prototype.<encoded method name>
   *    \$c_<encoded class name>.<encoded method name>
   *    \$b_<encoded class name>.<encoded method name>
   *    \$s_<encoded class name>__<encoded method name>
   *    \$f_<encoded class name>__<encoded method name>
   *    \$m_<encoded module name>
   *  }}}
   *  all of them optionally prefixed by `Object.`, `[object Object].` or
   *  `Module.`. (it comes after the "new " for the patterns where it start with
   *  a "new ")
   *
   *  When the function name is none of those, the pair
   *    `("<jscode>", functionName)`
   *  is returned, which will instruct [[StackTraceElement.toString()]] to only
   *  display the function name.
   *
   *  @return
   *    A 2-element array with the recovered class and method names, in that
   *    order. It is an array instead of a tuple because tuples have user code
   *    in the Scala.js standard library, which we cannot reference from the
   *    javalanglib.
   */
  private def extractClassMethod(functionName: String): js.Array[String] = {
    val PatBC = """^(?:Object\.|\[object Object\]\.|Module\.)?\$[bc]_([^\.]+)(?:\.prototype)?\.([^\.]+)$""".re
    val PatS = """^(?:Object\.|\[object Object\]\.|Module\.)?\$(?:ps?|s|f)_((?:_[^_]|[^_])+)__([^\.]+)$""".re
    val PatCT = """^(?:Object\.|\[object Object\]\.|Module\.)?\$ct_((?:_[^_]|[^_])+)__([^\.]*)$""".re
    val PatN = """^new (?:Object\.|\[object Object\]\.|Module\.)?\$c_([^\.]+)$""".re
    val PatM = """^(?:Object\.|\[object Object\]\.|Module\.)?\$m_([^\.]+)$""".re

    val matchBC = PatBC.exec(functionName)
    val matchBCOrS = if (matchBC ne null) matchBC else PatS.exec(functionName)
    if (matchBCOrS ne null) {
      js.Array[String](decodeClassName(undefOrForceGet(matchBCOrS(1))),
          decodeMethodName(undefOrForceGet(matchBCOrS(2))))
    } else {
      val matchCT = PatCT.exec(functionName)
      val matchCTOrN = if (matchCT ne null) matchCT else PatN.exec(functionName)
      if (matchCTOrN ne null) {
        js.Array[String](decodeClassName(undefOrForceGet(matchCTOrN(1))), "<init>")
      } else {
        val matchM = PatM.exec(functionName)
        if (matchM ne null) {
          js.Array[String](decodeClassName(undefOrForceGet(matchM(1))), "<clinit>")
        } else {
          js.Array[String]("<jscode>", functionName)
        }
      }
    }
  }

  // decodeClassName -----------------------------------------------------------

  private def decodeClassName(encodedName: String): String = {
    val base = if (dictContains(decompressedClasses, encodedName)) {
      dictRawApply(decompressedClasses, encodedName)
    } else {
      @tailrec
      def loop(i: Int): String = {
        if (i < compressedPrefixes.length) {
          val prefix = compressedPrefixes(i)
          if (encodedName.startsWith(prefix))
            dictRawApply(decompressedPrefixes, prefix) + encodedName.substring(prefix.length)
          else
            loop(i+1)
        } else {
          // no prefix matches
          if (encodedName.startsWith("L")) encodedName.substring(1)
          else encodedName // just in case
        }
      }
      loop(0)
    }
    base.replace("_", ".").replace("\uff3f", "_")
  }

  private lazy val decompressedClasses: js.Dictionary[String] = {
    val dict = new js.Object().asInstanceOf[js.Dictionary[String]]
    dictSet(dict, "O", "java_lang_Object")
    dictSet(dict, "T", "java_lang_String")

    var index = 0
    while (index <= 22) {
      if (index >= 2)
        dictSet(dict, s"T$index", s"scala_Tuple$index")
      dictSet(dict, s"F$index", s"scala_Function$index")
      index += 1
    }

    dict
  }

  private lazy val decompressedPrefixes: js.Dictionary[String] = {
    val dict = new js.Object().asInstanceOf[js.Dictionary[String]]
    dictSet(dict, "sjsr_", "scala_scalajs_runtime_")
    dictSet(dict, "sjs_", "scala_scalajs_")
    dictSet(dict, "sci_", "scala_collection_immutable_")
    dictSet(dict, "scm_", "scala_collection_mutable_")
    dictSet(dict, "scg_", "scala_collection_generic_")
    dictSet(dict, "sc_", "scala_collection_")
    dictSet(dict, "sr_", "scala_runtime_")
    dictSet(dict, "s_", "scala_")
    dictSet(dict, "jl_", "java_lang_")
    dictSet(dict, "ju_", "java_util_")
    dict
  }

  private lazy val compressedPrefixes =
    js.Object.keys(decompressedPrefixes.asInstanceOf[js.Object])

  // end of decodeClassName ----------------------------------------------------

  private def decodeMethodName(encodedName: String): String = {
    if (encodedName startsWith "init___") {
      "<init>"
    } else {
      val methodNameLen = encodedName.indexOf("__")
      if (methodNameLen < 0) encodedName
      else encodedName.substring(0, methodNameLen)
    }
  }

  private implicit class StringRE(private val s: String) extends AnyVal {
    def re: js.RegExp = new js.RegExp(s)
    def re(mods: String): js.RegExp = new js.RegExp(s, mods)
  }

  /* ---------------------------------------------------------------------------
   * Start copy-paste-translate from stacktrace.js
   *
   * From here on, most of the code has been copied from
   * https://github.com/stacktracejs/stacktrace.js
   * and translated to Scala.js almost literally, with some adaptations.
   *
   * Most comments -and lack thereof- have also been copied therefrom.
   */

  private def normalizeStackTraceLines(e: js.Dynamic): js.Array[String] = {
    import js.DynamicImplicits.{truthValue, number2dynamic}

    /* You would think that we could test once and for all which "mode" to
     * adopt. But the format can actually differ for different exceptions
     * on some browsers, e.g., exceptions in Chrome there may or may not have
     * arguments or stack.
     */

    if (!e) {
      js.Array[String]()
    } else if (e.arguments && e.stack) {
      extractChrome(e)
    } else if (e.stack && e.sourceURL) {
      extractSafari(e)
    } else if (e.stack && e.number) {
      extractIE(e)
    } else if (e.stack && e.fileName) {
      extractFirefox(e)
    } else if (e.message && e.`opera#sourceloc`) {
      // e.message.indexOf("Backtrace:") > -1 -> opera9
      // 'opera#sourceloc' in e -> opera9, opera10a
      // !e.stacktrace -> opera9
      @inline def messageIsLongerThanStacktrace =
        e.message.split("\n").length > e.stacktrace.split("\n").length
      if (!e.stacktrace) {
        extractOpera9(e) // use e.message
      } else if ((e.message.indexOf("\n") > -1) && messageIsLongerThanStacktrace) {
        // e.message may have more stack entries than e.stacktrace
        extractOpera9(e) // use e.message
      } else {
        extractOpera10a(e) // use e.stacktrace
      }
    } else if (e.message && e.stack && e.stacktrace) {
      // stacktrace && stack -> opera10b
      if (e.stacktrace.indexOf("called from line") < 0) {
        extractOpera10b(e)
      } else {
        extractOpera11(e)
      }
    } else if (e.stack && !e.fileName) {
      /* Chrome 27 does not have e.arguments as earlier versions,
       * but still does not have e.fileName as Firefox */
      extractChrome(e)
    } else {
      extractOther(e)
    }
  }

  private def extractChrome(e: js.Dynamic): js.Array[String] = {
    (e.stack.asInstanceOf[String] + "\n")
      .jsReplace("""^[\s\S]+?\s+at\s+""".re, " at ") // remove message
      .jsReplace("""^\s+(at eval )?at\s+""".re("gm"), "") // remove 'at' and indentation
      .jsReplace("""^([^\(]+?)([\n])""".re("gm"), "{anonymous}() ($1)$2") // see note
      .jsReplace("""^Object.<anonymous>\s*\(([^\)]+)\)""".re("gm"), "{anonymous}() ($1)")
      .jsReplace("""^([^\(]+|\{anonymous\}\(\)) \((.+)\)$""".re("gm"), "$1@$2")
      .jsSplit("\n")
      .jsSlice(0, -1)

    /* Note: there was a $ next to the \n here in the original code, but it
     * chokes with method names with $'s, which are generated often by Scala.js.
     */
  }

  private def extractFirefox(e: js.Dynamic): js.Array[String] = {
    (e.stack.asInstanceOf[String])
      .jsReplace("""(?:\n@:0)?\s+$""".re("m"), "")
      .jsReplace("""^(?:\((\S*)\))?@""".re("gm"), "{anonymous}($1)@")
      .jsSplit("\n")
  }

  private def extractIE(e: js.Dynamic): js.Array[String] = {
    (e.stack.asInstanceOf[String])
      .jsReplace("""^\s*at\s+(.*)$""".re("gm"), "$1")
      .jsReplace("""^Anonymous function\s+""".re("gm"), "{anonymous}() ")
      .jsReplace("""^([^\(]+|\{anonymous\}\(\))\s+\((.+)\)$""".re("gm"), "$1@$2")
      .jsSplit("\n")
      .jsSlice(1)
  }

  private def extractSafari(e: js.Dynamic): js.Array[String] = {
    (e.stack.asInstanceOf[String])
      .jsReplace("""\[native code\]\n""".re("m"), "")
      .jsReplace("""^(?=\w+Error\:).*$\n""".re("m"), "")
      .jsReplace("""^@""".re("gm"), "{anonymous}()@")
      .jsSplit("\n")
  }

  private def extractOpera9(e: js.Dynamic): js.Array[String] = {
    // "  Line 43 of linked script file://localhost/G:/js/stacktrace.js\n"
    // "  Line 7 of inline#1 script in file://localhost/G:/js/test/functional/testcase1.html\n"
    val lineRE = """Line (\d+).*script (?:in )?(\S+)""".re("i")
    val lines = (e.message.asInstanceOf[String]).jsSplit("\n")
    val result = new js.Array[String]

    var i = 2
    val len = lines.length
    while (i < len) {
      val mtch = lineRE.exec(lines(i))
      if (mtch ne null) {
        result.push(
            "{anonymous}()@" + undefOrForceGet(mtch(2)) + ":" +
            undefOrForceGet(mtch(1))
            /* + " -- " + lines(i+1).replace("""^\s+""".re, "") */)
      }
      i += 2
    }

    result
  }

  private def extractOpera10a(e: js.Dynamic): js.Array[String] = {
    // "  Line 27 of linked script file://localhost/G:/js/stacktrace.js\n"
    // "  Line 11 of inline#1 script in file://localhost/G:/js/test/functional/testcase1.html: In function foo\n"
    val lineRE = """Line (\d+).*script (?:in )?(\S+)(?:: In function (\S+))?$""".re("i")
    val lines = (e.stacktrace.asInstanceOf[String]).jsSplit("\n")
    val result = new js.Array[String]

    var i = 0
    val len = lines.length
    while (i < len) {
      val mtch = lineRE.exec(lines(i))
      if (mtch ne null) {
        val fnName = undefOrGetOrElse(mtch(3))("{anonymous}")
        result.push(
            fnName + "()@" + undefOrForceGet(mtch(2)) + ":" +
            undefOrForceGet(mtch(1))
            /* + " -- " + lines(i+1).replace("""^\s+""".re, "")*/)
      }
      i += 2
    }

    result
  }

  private def extractOpera10b(e: js.Dynamic): js.Array[String] = {
    // "<anonymous function: run>([arguments not available])@file://localhost/G:/js/stacktrace.js:27\n" +
    // "printStackTrace([arguments not available])@file://localhost/G:/js/stacktrace.js:18\n" +
    // "@file://localhost/G:/js/test/functional/testcase1.html:15"
    val lineRE = """^(.*)@(.+):(\d+)$""".re
    val lines = (e.stacktrace.asInstanceOf[String]).jsSplit("\n")
    val result = new js.Array[String]

    var i = 0
    val len = lines.length
    while (i < len) {
      val mtch = lineRE.exec(lines(i))
      if (mtch ne null) {
        val fnName = undefOrFold(mtch(1))("global code")(_ + "()")
        result.push(fnName + "@" + undefOrForceGet(mtch(2)) + ":" + undefOrForceGet(mtch(3)))
      }
      i += 1
    }

    result
  }

  private def extractOpera11(e: js.Dynamic): js.Array[String] = {
    val lineRE = """^.*line (\d+), column (\d+)(?: in (.+))? in (\S+):$""".re
    val lines = (e.stacktrace.asInstanceOf[String]).jsSplit("\n")
    val result = new js.Array[String]

    var i = 0
    val len = lines.length
    while (i < len) {
      val mtch = lineRE.exec(lines(i))
      if (mtch ne null) {
        val location = undefOrForceGet(mtch(4)) + ":" + undefOrForceGet(mtch(1)) + ":" + undefOrForceGet(mtch(2))
        val fnName0 = undefOrGetOrElse(mtch(2))("global code")
        val fnName = fnName0
          .jsReplace("""<anonymous function: (\S+)>""".re, "$1")
          .jsReplace("""<anonymous function>""".re, "{anonymous}")
        result.push(fnName + "@" + location
            /* + " -- " + lines(i+1).replace("""^\s+""".re, "")*/)
      }
      i += 2
    }

    result
  }

  private def extractOther(e: js.Dynamic): js.Array[String] = {
    js.Array()
  }

  /* End copy-paste-translate from stacktrace.js
   * ---------------------------------------------------------------------------
   */

}
