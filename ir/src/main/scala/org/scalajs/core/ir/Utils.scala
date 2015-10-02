/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js IR                **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.ir

import java.net.URI

import scala.annotation.switch

object Utils {

  /** Relativize target URI w.r.t. base URI */
  def relativize(base0: URI, trgt0: URI): URI = {
    val base = base0.normalize
    val trgt = trgt0.normalize

    if (base.isOpaque || !base.isAbsolute || base.getRawPath == null ||
        trgt.isOpaque || !trgt.isAbsolute || trgt.getRawPath == null ||
        base.getScheme != trgt.getScheme  ||
        base.getRawAuthority != trgt.getRawAuthority)
      trgt
    else {
      val trgtCmps = trgt.getRawPath.split('/')
      val baseCmps = base.getRawPath.split('/')

      val prefixLen = (trgtCmps zip baseCmps).takeWhile(t => t._1 == t._2).size

      val newPathCmps =
        List.fill(baseCmps.size - prefixLen)("..") ++ trgtCmps.drop(prefixLen)

      val newPath = newPathCmps.mkString("/")

      // Relative URI does not have scheme or authority
      new URI(null, null, newPath, trgt.getRawQuery, trgt.getRawFragment)
    }
  }

  /** Adds an empty authority to URIs with the "file" scheme without authority.
   *  Some browsers don't fetch URIs without authority correctly.
   */
  def fixFileURI(uri: URI): URI =
    if (uri.getScheme() != "file" || uri.getAuthority() != null) uri
    else new URI("file", "", uri.getPath(), uri.getQuery(), uri.getFragment())

  def escapeJS(str: String): String = {
    // scalastyle:off return
    /* Note that Java and JavaScript happen to use the same encoding for
     * Unicode, namely UTF-16, which means that 1 char from Java always equals
     * 1 char in JavaScript. */
    val end = str.length
    var i = 0
    while (i != end) {
      val c = str.charAt(i)
      if (c >= 32 && c <= 126 && c != '\\' && c != '"')
        i += 1
      else
        return createEscapeJSString(str)
    }
    str
    // scalastyle:on return
  }

  private def createEscapeJSString(str: String): String = {
    val end = str.length()
    val builder = new java.lang.StringBuilder
    builder.ensureCapacity(end * 2)
    var i = 0
    while (i != end) {
      val c = str.charAt(i)
      if (c >= 32 && c <= 126 && c != '\\' && c != '"') {
        builder.append(c) // ASCII printable characters
      } else {
        def encode(c: Char): String = f"\\u$c%04x"
        builder.append((c: @switch) match {
          case '\\'     => "\\\\"
          case '"'      => "\\\""
          case '\u0007' => "\\a"
          case '\u0008' => "\\b"
          case '\u0009' => "\\t"
          case '\u000A' => "\\n"
          case '\u000B' => "\\v"
          case '\u000C' => "\\f"
          case '\u000D' => "\\r"
          case c        => encode(c)
        })
      }
      i += 1
    }
    builder.toString()
  }

  /** A ByteArrayOutput stream that allows to jump back to a given
   *  position and complete some bytes. Methods must be called in the
   *  following order only:
   *  - [[markJump]]
   *  - [[jumpBack]]
   *  - [[continue]]
   */
  private[ir] class JumpBackByteArrayOutputStream
      extends java.io.ByteArrayOutputStream {
    protected var jumpBackPos: Int = -1
    protected var headPos: Int = -1

    /** Marks the current location for a jumpback */
    def markJump(): Unit = {
      assert(jumpBackPos == -1)
      assert(headPos == -1)
      jumpBackPos = count
    }

    /** Jumps back to the mark. Returns the number of bytes jumped */
    def jumpBack(): Int = {
      assert(jumpBackPos >= 0)
      assert(headPos == -1)
      val jumped = count - jumpBackPos
      headPos = count
      count = jumpBackPos
      jumpBackPos = -1
      jumped
    }

    /** Continues to write at the head. */
    def continue(): Unit = {
      assert(jumpBackPos == -1)
      assert(headPos >= 0)
      count = headPos
      headPos = -1
    }
  }

}
