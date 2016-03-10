package scala.scalajs.runtime

import scala.annotation.tailrec

import scala.scalajs.js
import js.JSStringOps._

/** Conversions of JavaScript stack traces to Java stack traces.
 */
object StackTrace {

  /* !!! Note that in this unit, we go to great lengths *not* to use anything
   * from the Scala collections library.
   *
   * This minimizes the risk of runtime errors during the process of decoding
   * errors, which would be very bad if it happened.
   */

  import Implicits._

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
    if (js.isUndefined(js.constructorOf[js.Error].captureStackTrace)) {
      captureState(throwable, createException())
    } else {
      /* V8-specific.
       * The Error.captureStackTrace(e) method records the current stack trace
       * on `e` as would do `new Error()`, thereby turning `e` into a proper
       * exception. This avoids creating a dummy exception, but is mostly
       * important so that Node.js will show stack traces if the exception
       * is never caught and reaches the global event queue.
       */
      js.constructorOf[js.Error].captureStackTrace(throwable.asInstanceOf[js.Any])
      captureState(throwable, throwable)
    }
  }

  /** Creates a JS Error with the current stack trace state. */
  @inline private def createException(): Any = {
    try {
      // Intentionally throw a JavaScript error
      new js.Object().asInstanceOf[js.Dynamic].undef()
    } catch {
      case js.JavaScriptException(e) => e
    }
  }

  /** Captures browser-specific state recording the stack trace of a JS error.
   *  The state is stored as a magic field of the throwable, and will be used
   *  by `extract()` to create an Array[StackTraceElement].
   */
  @inline def captureState(throwable: Throwable, e: Any): Unit =
    throwable.asInstanceOf[js.Dynamic].stackdata = e.asInstanceOf[js.Any]

  /** Tests whether we're running under Rhino. */
  private lazy val isRhino: Boolean = {
    try {
      js.Dynamic.global.Packages.org.mozilla.javascript.JavaScriptException
      true
    } catch {
      case js.JavaScriptException(_) => false
    }
  }

  /** Extracts a throwable's stack trace from captured browser-specific state.
   *  If no stack trace state has been recorded, or if the state cannot be
   *  analyzed in meaningful way (because we don't know the browser), an
   *  empty array is returned.
   */
  def extract(throwable: Throwable): Array[StackTraceElement] =
    extract(throwable.asInstanceOf[js.Dynamic].stackdata)

  /** Extracts a stack trace from captured browser-specific stackdata.
   *  If no stack trace state has been recorded, or if the state cannot be
   *  analyzed in meaningful way (because we don't know the browser), an
   *  empty array is returned.
   */
  def extract(stackdata: js.Dynamic): Array[StackTraceElement] = {
    val lines = normalizeStackTraceLines(stackdata)
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

    val trace = new js.Array[JSStackTraceElem]
    var i = 0
    while (i < lines.length) {
      val line = lines(i)
      if (!line.isEmpty) {
        val mtch1 = NormalizedFrameLineWithColumn.exec(line)
        if (mtch1 ne null) {
          val (className, methodName) = extractClassMethod(mtch1(1).get)
          trace.push(JSStackTraceElem(className, methodName, mtch1(2).get,
              mtch1(3).get.toInt, mtch1(4).get.toInt))
        } else {
          val mtch2 = NormalizedFrameLine.exec(line)
          if (mtch2 ne null) {
            val (className, methodName) = extractClassMethod(mtch2(1).get)
            trace.push(JSStackTraceElem(className,
                methodName, mtch2(2).get, mtch2(3).get.toInt))
          } else {
            // just in case
            trace.push(JSStackTraceElem("<jscode>", line, null, -1))
          }
        }
      }
      i += 1
    }

    // Map stack trace through environment (if supported)
    val mappedTrace =
      environmentInfo.sourceMapper.fold(trace)(mapper => mapper(trace))

    // Convert JS objects to java.lang.StackTraceElements
    // While loop due to space concerns
    val result = new Array[StackTraceElement](mappedTrace.length)

    i = 0
    while (i < mappedTrace.length) {
      val jsSte = mappedTrace(i)
      val ste = new StackTraceElement(jsSte.declaringClass, jsSte.methodName,
          jsSte.fileName, jsSte.lineNumber)
      jsSte.columnNumber.foreach(ste.setColumnNumber)
      result(i) = ste
      i += 1
    }

    result
  }

  /** Tries and extract the class name and method from the JS function name.
   *
   *  The recognized patterns are
   *  {{{
   *    \$c_<encoded class name>.prototype.<encoded method name>
   *    \$c_<encoded class name>.<encoded method name>
   *    \$s_<encoded class name>__<encoded method name>
   *    \$m_<encoded module name>
   *  }}}
   *  and their ECMAScript51Global equivalents:
   *  {{{
   *    ScalaJS.c.<encoded class name>.prototype.<encoded method name>
   *    ScalaJS.c.<encoded class name>.<encoded method name>
   *    ScalaJS.s.<encoded class name>__<encoded method name>
   *    ScalaJS.m.<encoded module name>
   *  }}}
   *  all of them optionally prefixed by `Object.` or `[object Object].`.
   *
   *  When the function name is none of those, the pair
   *    `("<jscode>", functionName)`
   *  is returned, which will instruct [[StackTraceElement.toString()]] to only
   *  display the function name.
   */
  private def extractClassMethod(functionName: String): (String, String) = {
    val PatC = """^(?:Object\.|\[object Object\]\.)?(?:ScalaJS\.c\.|\$c_)([^\.]+)(?:\.prototype)?\.([^\.]+)$""".re
    val PatS = """^(?:Object\.|\[object Object\]\.)?(?:ScalaJS\.(?:s|f)\.|\$(?:s|f)_)((?:_[^_]|[^_])+)__([^\.]+)$""".re
    val PatM = """^(?:Object\.|\[object Object\]\.)?(?:ScalaJS\.m\.|\$m_)([^\.]+)$""".re

    var isModule = false
    var mtch = PatC.exec(functionName)
    if (mtch eq null) {
      mtch = PatS.exec(functionName)
      if (mtch eq null) {
        mtch = PatM.exec(functionName)
        isModule = true
      }
    }

    if (mtch ne null) {
      val className = decodeClassName(mtch(1).get)
      val methodName = if (isModule)
        "<clinit>" // that's how it would be reported on the JVM
      else
        decodeMethodName(mtch(2).get)
      (className, methodName)
    } else {
      ("<jscode>", functionName)
    }
  }

  // decodeClassName -----------------------------------------------------------

  // !!! Duplicate logic: this code must be in sync with ir.Definitions

  private def decodeClassName(encodedName: String): String = {
    val encoded =
      if (encodedName.charAt(0) == '$') encodedName.substring(1)
      else encodedName
    val base = if (decompressedClasses.contains(encoded)) {
      decompressedClasses(encoded)
    } else {
      @tailrec
      def loop(i: Int): String = {
        if (i < compressedPrefixes.length) {
          val prefix = compressedPrefixes(i)
          if (encoded.startsWith(prefix))
            decompressedPrefixes(prefix) + encoded.substring(prefix.length)
          else
            loop(i+1)
        } else {
          // no prefix matches
          if (encoded.startsWith("L")) encoded.substring(1)
          else encoded // just in case
        }
      }
      loop(0)
    }
    base.replace("_", ".").replace("$und", "_")
  }

  private lazy val decompressedClasses: js.Dictionary[String] = {
    val dict = js.Dynamic.literal(
        O = "java_lang_Object",
        T = "java_lang_String",
        V = "scala_Unit",
        Z = "scala_Boolean",
        C = "scala_Char",
        B = "scala_Byte",
        S = "scala_Short",
        I = "scala_Int",
        J = "scala_Long",
        F = "scala_Float",
        D = "scala_Double"
    ).asInstanceOf[js.Dictionary[String]]

    var index = 0
    while (index <= 22) {
      if (index >= 2)
        dict("T"+index) = "scala_Tuple"+index
      dict("F"+index) = "scala_Function"+index
      index += 1
    }

    dict
  }

  private lazy val decompressedPrefixes = js.Dynamic.literal(
      sjsr_ = "scala_scalajs_runtime_",
      sjs_  = "scala_scalajs_",
      sci_  = "scala_collection_immutable_",
      scm_  = "scala_collection_mutable_",
      scg_  = "scala_collection_generic_",
      sc_   = "scala_collection_",
      sr_   = "scala_runtime_",
      s_    = "scala_",
      jl_   = "java_lang_",
      ju_   = "java_util_"
  ).asInstanceOf[js.Dictionary[String]]

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

  private implicit class StringRE(val s: String) extends AnyVal {
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

    /* Ideally we would write tests like
     *   if (e.arguments && e.stack)
     * but Scala 2.10 does not like that (too much Dynamic magic, I suppose).
     * So we define inlineable defs for the fields we're interested in.
     * This way we make the compiler happy
     */
    @inline def arguments = e.arguments
    @inline def stack = e.stack
    @inline def sourceURL = e.sourceURL
    @inline def number = e.number
    @inline def fileName = e.fileName
    @inline def message = e.message
    @inline def `opera#sourceloc` = e.`opera#sourceloc`
    @inline def stacktrace = e.stacktrace

    if (!e) {
      js.Array[String]()
    } else if (isRhino) {
      extractRhino(e)
    } else if (arguments && stack) {
      extractChrome(e)
    } else if (stack && sourceURL) {
      extractSafari(e)
    } else if (stack && number) {
      extractIE(e)
    } else if (stack && fileName) {
      extractFirefox(e)
    } else if (message && `opera#sourceloc`) {
      // e.message.indexOf("Backtrace:") > -1 -> opera9
      // 'opera#sourceloc' in e -> opera9, opera10a
      // !e.stacktrace -> opera9
      @inline def messageIsLongerThanStacktrace =
        message.split("\n").length > stacktrace.split("\n").length
      if (!stacktrace) {
        extractOpera9(e) // use e.message
      } else if ((message.indexOf("\n") > -1) && messageIsLongerThanStacktrace) {
        // e.message may have more stack entries than e.stacktrace
        extractOpera9(e) // use e.message
      } else {
        extractOpera10a(e) // use e.stacktrace
      }
    } else if (message && stack && stacktrace) {
      // stacktrace && stack -> opera10b
      if (stacktrace.indexOf("called from line") < 0) {
        extractOpera10b(e)
      } else {
        extractOpera11(e)
      }
    } else if (stack && !fileName) {
      /* Chrome 27 does not have e.arguments as earlier versions,
       * but still does not have e.fileName as Firefox */
      extractChrome(e)
    } else {
      extractOther(e)
    }
  }

  private def extractRhino(e: js.Dynamic): js.Array[String] = {
    (e.stack.asInstanceOf[js.UndefOr[String]]).getOrElse("")
      .jsReplace("""^\s+at\s+""".re("gm"), "") // remove 'at' and indentation
      .jsReplace("""^(.+?)(?: \((.+)\))?$""".re("gm"), "$2@$1")
      .jsReplace("""\r\n?""".re("gm"), "\n") // Rhino has platform-dependent EOL's
      .jsSplit("\n")
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
    val len = lines.length.toInt
    while (i < len) {
      val mtch = lineRE.exec(lines(i))
      if (mtch ne null) {
        result.push("{anonymous}()@" + mtch(2).get + ":" + mtch(1).get
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
    val len = lines.length.toInt
    while (i < len) {
      val mtch = lineRE.exec(lines(i))
      if (mtch ne null) {
        val fnName = mtch(3).getOrElse("{anonymous}")
        result.push(fnName + "()@" + mtch(2).get + ":" + mtch(1).get
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
    val len = lines.length.toInt
    while (i < len) {
      val mtch = lineRE.exec(lines(i))
      if (mtch ne null) {
        val fnName = mtch(1).fold("global code")(_ + "()")
        result.push(fnName + "@" + mtch(2).get + ":" + mtch(3).get)
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
    val len = lines.length.toInt
    while (i < len) {
      val mtch = lineRE.exec(lines(i))
      if (mtch ne null) {
        val location = mtch(4).get + ":" + mtch(1).get + ":" + mtch(2).get
        val fnName0 = mtch(2).getOrElse("global code")
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

  @js.native
  trait JSStackTraceElem extends js.Object {
    var declaringClass: String = js.native
    var methodName: String = js.native
    var fileName: String = js.native
    /** 1-based line number */
    var lineNumber: Int = js.native
    /** 1-based optional columnNumber */
    var columnNumber: js.UndefOr[Int] = js.native
  }

  object JSStackTraceElem {
    @inline
    def apply(declaringClass: String, methodName: String,
        fileName: String, lineNumber: Int,
        columnNumber: js.UndefOr[Int] = js.undefined): JSStackTraceElem = {
      js.Dynamic.literal(
          declaringClass = declaringClass,
          methodName = methodName,
          fileName = fileName,
          lineNumber = lineNumber,
          columnNumber = columnNumber
      ).asInstanceOf[JSStackTraceElem]
    }
  }

  /**
   *  Implicit class to access magic column element created in STE
   */
  @deprecated("Use Implicits.StackTraceElementOps instead.", "0.6.3")
  implicit class ColumnStackTraceElement(ste: StackTraceElement) {
    def getColumnNumber: Int =
      new Implicits.StackTraceElementOps(ste).getColumnNumber()
  }

  object Implicits {
    /** Access to the additional methods `getColumnNumber` and `setColumnNumber`
     *  of [[java.lang.StackTraceElement StackTraceElement]].
     */
    implicit class StackTraceElementOps(
        val ste: StackTraceElement) extends AnyVal {
      @inline
      def getColumnNumber(): Int =
        ste.asInstanceOf[js.Dynamic].getColumnNumber().asInstanceOf[Int]

      @inline
      def setColumnNumber(columnNumber: Int): Unit =
        ste.asInstanceOf[js.Dynamic].setColumnNumber(columnNumber)
    }
  }

}
