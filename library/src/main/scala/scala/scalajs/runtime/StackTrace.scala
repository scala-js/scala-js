package scala.scalajs.runtime

import scala.scalajs.js

/** Conversions of JavaScript stack traces to Java stack traces.
 *
 *  Inspired and sometimes copied from
 *  https://github.com/stacktracejs/stacktrace.js
 */
object StackTrace {

  def captureState(throwable: Throwable): Unit = {
    captureState(throwable, createException())
  }

  private def createException(): js.Any = {
    try {
      this.asInstanceOf[js.Dynamic].undef() // it does not exist, that's the point
    } catch {
      case js.JavaScriptException(e) => e
    }
  }

  def captureState(throwable: Throwable, e: js.Any): Unit = {
    throwable.asInstanceOf[js.Dynamic].stackdata = e
  }

  private lazy val isRhino: Boolean = {
    try {
      js.Dynamic.global.Packages.org.mozilla.javascript.JavaScriptException
      true
    } catch {
      case js.JavaScriptException(_) => false
    }
  }

  def extract(throwable: Throwable): Array[StackTraceElement] = {
    /* Mode could differ for different exception, e.g., exceptions in Chrome
     * may or may not have arguments or stack. */
    val e = throwable.asInstanceOf[js.Dynamic].stackdata

    val normalizedLines = if (!e) {
      js.Array[js.String]()
    } else if (isRhino) {
      extractRhino(e)
    } else if (!(!e.arguments) && !(!e.stack)) {
      extractChrome(e)
    } else if (!(!e.stack) && !(!e.sourceURL)) {
      extractSafari(e)
    } else if (!(!e.stack) && !(!e.number)) {
      extractIE(e)
    } else if (!(!e.stack) && !(!e.fileName)) {
      extractFirefox(e)
    } else if (!(!e.message) && !(!e.`opera#sourceloc`)) {
      // e.message.indexOf("Backtrace:") > -1 -> opera9
      // 'opera#sourceloc' in e -> opera9, opera10a
      // !e.stacktrace -> opera9
      if (!e.stacktrace) {
        extractOpera9(e) // use e.message
      } else if ((e.message.indexOf("\n") > -1) &&
          (e.message.split("\n").length > e.stacktrace.split("\n").length)) {
        // e.message may have more stack entries than e.stacktrace
        extractOpera9(e) // use e.message
      } else {
        extractOpera10a(e) // use e.stacktrace
      }
    } else if (!(!e.message) && !(!e.stack) && !(!e.stacktrace)) {
      // e.stacktrace && e.stack -> opera10b
      if (e.stacktrace.indexOf("called from line") < 0) {
        extractOpera10b(e)
      } else {
        extractOpera11(e)
      }
    } else if (!(!e.stack) && !e.fileName) {
      /* Chrome 27 does not have e.arguments as earlier versions,
       * but still does not have e.fileName as Firefox */
      extractChrome(e)
    } else {
      extractOther(e)
    }

    val NormalizedFrameLine = """^([^\@]*)\@(.*):([0-9]+)$""".re
    val NormalizedFrameLineWithColumn = """^([^\@]*)\@(.*):([0-9]+):([0-9]+)$""".re

    val result = new js.Array[StackTraceElement]
    var i = 0
    while (i < normalizedLines.length) {
      val line = normalizedLines(i)
      if (!(!line)) {
        val mtch1 = NormalizedFrameLineWithColumn.exec(line)
        if (mtch1 ne null) {
          val (className, methodName) = extractClassMethod(mtch1(1))
          result.push(
              STE(className, methodName, mtch1(2), mtch1(3).toInt, mtch1(4).toInt))
        } else {
          val mtch2 = NormalizedFrameLine.exec(line)
          if (mtch2 ne null) {
            val (className, methodName) = extractClassMethod(mtch2(1))
            result.push(
                STE(className, methodName, mtch2(2), mtch2(3).toInt))
          } else {
            result.push(STE("<jscode>", line, null, -1)) // just in case
          }
        }
      }
      i += 1
    }
    result
  }

  private def extractRhino(e: js.Dynamic): js.Array[js.String] = {
    (e.stack.asInstanceOf[js.String])
      .replace("""^\s+at\s+""".re("gm"), "") // remove 'at' and indentation
      .replace("""^(.+) \((.+)\)$""".re("gm"), "$2@$1")
      .split("\n")
  }

  private def extractChrome(e: js.Dynamic): js.Array[js.String] = {
    (e.stack.asInstanceOf[js.String] + "\n")
      .replace("""^[\s\S]+?\s+at\s+""".re, " at ") // remove message
      .replace("""^\s+(at eval )?at\s+""".re("gm"), "") // remove 'at' and indentation
      //.replace("""^([^\(]+?)([\n$])""".re("gm"), "{anonymous}() ($1)$2")
      //.replace("""^Object.<anonymous>\s*\(([^\)]+)\)""".re("gm"), "{anonymous}() ($1)")
      .replace("""^(.+) \((.+)\)$""".re("gm"), "$1@$2")
      .split("\n")
      .slice(0, -1)
  }

  private def extractFirefox(e: js.Dynamic): js.Array[js.String] = {
    (e.stack.asInstanceOf[js.String])
      .replace("""(?:\n@:0)?\s+$""".re("m"), "")
      //.replace("""^(?:\((\S*)\))?@""".re("gm"), "{anonymous}($1)@")
      .split("\n")
  }

  private def extractIE(e: js.Dynamic): js.Array[js.String] = {
    (e.stack.asInstanceOf[js.String])
      .replace("""^\s*at\s+(.*)$""".re("gm"), "$1")
      //.replace("""^Anonymous function\s+""".re("gm"), "{anonymous}() ")
      .replace("""^(.+)\s+\((.+)\)$""".re("gm"), "$1@$2")
      .split("\n")
      .slice(1)
  }

  private def extractSafari(e: js.Dynamic): js.Array[js.String] = {
    (e.stack.asInstanceOf[js.String])
      .replace("""\[native code\]\n""".re("m"), "")
      .replace("""^(?=\w+Error\:).*$\n""".re("m"), "")
      //.replace("""^@""".re("gm"), "{anonymous}()@")
      .split("\n")
  }

  private def extractOpera9(e: js.Dynamic): js.Array[js.String] = {
    // "  Line 43 of linked script file://localhost/G:/js/stacktrace.js\n"
    // "  Line 7 of inline#1 script in file://localhost/G:/js/test/functional/testcase1.html\n"
    val lineRE = """Line (\d+).*script (?:in )?(\S+)""".re("i")
    val lines = (e.message.asInstanceOf[js.String]).split("\n")
    val result = new js.Array[js.String]

    var i = 2
    val len = lines.length.toInt
    while (i < len) {
      val mtch = lineRE.exec(lines(i))
      if (mtch ne null) {
        /*result.push("{anonymous}()@" + mtch(2) + ":" + mtch(1) + " -- " +
            lines(i+1).replace("""^\s+""".re, ""))*/
        result.push("@" + mtch(2) + ":" + mtch(1))
      }
      i += 2
    }

    result
  }

  private def extractOpera10a(e: js.Dynamic): js.Array[js.String] = {
    // "  Line 27 of linked script file://localhost/G:/js/stacktrace.js\n"
    // "  Line 11 of inline#1 script in file://localhost/G:/js/test/functional/testcase1.html: In function foo\n"
    val lineRE = """Line (\d+).*script (?:in )?(\S+)(?:: In function (\S+))?$""".re("i")
    val lines = (e.stacktrace.asInstanceOf[js.String]).split("\n")
    val result = new js.Array[js.String]

    var i = 0
    val len = lines.length.toInt
    while (i < len) {
      val mtch = lineRE.exec(lines(i))
      if (mtch ne null) {
        /*val fnName: js.String = if (!mtch(3)) "{anonymous}" else mtch(3)
        result.push(fnName + "()@" + mtch(2) + ":" + mtch(1) + " -- " +
            lines(i+1).replace("""^\s+""".re, ""))*/
        result.push(mtch(3) + "@" + mtch(2) + ":" + mtch(1))
      }
      i += 2
    }

    result
  }

  private def extractOpera10b(e: js.Dynamic): js.Array[js.String] = {
    // "<anonymous function: run>([arguments not available])@file://localhost/G:/js/stacktrace.js:27\n" +
    // "printStackTrace([arguments not available])@file://localhost/G:/js/stacktrace.js:18\n" +
    // "@file://localhost/G:/js/test/functional/testcase1.html:15"
    val lineRE = """^(.*)@(.+):(\d+)$""".re
    val lines = (e.stacktrace.asInstanceOf[js.String]).split("\n")
    val result = new js.Array[js.String]

    var i = 0
    val len = lines.length.toInt
    while (i < len) {
      val mtch = lineRE.exec(lines(i))
      if (mtch ne null) {
        /*val fnName: js.String = if (!mtch(1)) "global code" else mtch(1) + "()"
        result.push(fnName + "@" + mtch(2) + ":" + mtch(3))*/
        result.push(mtch(1) + "@" + mtch(2) + ":" + mtch(3))
      }
      i += 1
    }

    result
  }

  private def extractOpera11(e: js.Dynamic): js.Array[js.String] = {
    val lineRE = """^.*line (\d+), column (\d+)(?: in (.+))? in (\S+):$""".re
    val lines = (e.stacktrace.asInstanceOf[js.String]).split("\n")
    val result = new js.Array[js.String]

    var i = 0
    val len = lines.length.toInt
    while (i < len) {
      val mtch = lineRE.exec(lines(i))
      if (mtch ne null) {
        val location = mtch(4) + ":" + mtch(1) + ":" + mtch(2)
        val fnName0: js.String = if (!mtch(3)) "global code" else mtch(3)
        val fnName = fnName0
          .replace("""<anonymous function: (\S+)>""".re, "$1")
          //.replace("""<anonymous function>""".re, "{anonymous}")
        /*result.push(fnName + "@" + location + " -- " +
            lines(i+1).replace("""^\s+""".re, ""))*/
        result.push(fnName + "@" + location)
      }
      i += 2
    }

    result
  }

  private def extractOther(e: js.Dynamic): js.Array[js.String] = {
    js.Array()
  }

  private def STE(declaringClass: String, methodName: String,
      fileName: String, lineNumber: Int): StackTraceElement =
    new StackTraceElement(declaringClass, methodName, fileName, lineNumber)

  private def STE(declaringClass: String, methodName: String,
      fileName: String, lineNumber: Int, columnNumber: Int): StackTraceElement =
    new StackTraceElement(declaringClass, methodName, fileName, lineNumber)

  private implicit class StringRE(val s: String) extends AnyVal {
    def re: js.RegExp = new js.RegExp(s)
    def re(mods: String): js.RegExp = new js.RegExp(s, mods)
  }

  private def extractClassMethod(functionName: String): (String, String) = {
    val Pat = """^ScalaJS\.c\.([^\.]+)(\.prototype)?\.([^\.]+)$""".re
    val mtch = Pat.exec(functionName)
    if (mtch ne null) {
      val classEncoding: String = mtch(1)
      val methodEncoding: String = mtch(3)
      val className = classEncoding.replace("_", ".").replace("$und", "_")
      val methodName = {
        if (methodEncoding startsWith "init___") {
          "<init>"
        } else {
          val methodNameLen = methodEncoding.indexOf("__")
          if (methodNameLen < 0) methodEncoding
          else methodEncoding.substring(0, methodNameLen)
        }
      }
      (className, methodName)
    } else {
      ("<jscode>", functionName)
    }
  }
}
