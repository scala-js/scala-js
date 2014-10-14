/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js IR                **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.ir

import java.net.URI

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
    /* Note that Java and JavaScript happen to use the same encoding for
     * Unicode, namely UTF-16, which means that 1 char from Java always equals
     * 1 char in JavaScript. */
    val builder = new StringBuilder
    str foreach {
      case '\\'     => builder.append("\\\\")
      case '"'      => builder.append("\\\"")
      case '\u0007' => builder.append("\\a")
      case '\u0008' => builder.append("\\b")
      case '\u0009' => builder.append("\\t")
      case '\u000A' => builder.append("\\n")
      case '\u000B' => builder.append("\\v")
      case '\u000C' => builder.append("\\f")
      case '\u000D' => builder.append("\\r")
      case c =>
        if (c >= 32 && c <= 126) builder.append(c.toChar) // ASCII printable characters
        else builder.append(f"\\u$c%04x")
    }
    builder.result()
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
