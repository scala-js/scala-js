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

package java.net

import scala.scalajs.js.RegExp
import scala.scalajs.js

import scala.annotation.tailrec

import java.nio._
import java.nio.charset.{CodingErrorAction, StandardCharsets}

final class URI(origStr: String) extends Serializable with Comparable[URI] {

  import URI.Fields._
  import URI.decodeComponent
  import URI.quoteNonASCII

  /** The fields matched in the regular expression.
   *
   *  This is a local val for the primary constructor. It is a val,
   *  since we'll set it to null after initializing all fields.
   */
  private[this] var _fld: RegExp.ExecResult = URI.uriRe.exec(origStr)
  if (_fld == null)
    throw new URISyntaxException(origStr, "Malformed URI")

  private val _isAbsolute = _fld(AbsScheme).isDefined
  private val _isOpaque = _fld(AbsOpaquePart).isDefined

  @inline private def fld(idx: Int): String = _fld(idx).orNull

  @inline private def fld(absIdx: Int, relIdx: Int): String =
    if (_isAbsolute) fld(absIdx) else fld(relIdx)

  /** Nullable */
  private val _scheme = fld(AbsScheme)

  /** Non-nullable */
  private val _schemeSpecificPart = {
    if (!_isAbsolute) fld(RelSchemeSpecificPart)
    else if (_isOpaque) fld(AbsOpaquePart)
    else fld(AbsHierPart)
  }

  /** Nullable */
  private val _authority = {
    val authPart = fld(AbsAuthority, RelAuthority)
    if (authPart == "") null else authPart
  }

  /** Nullable */
  private val _userInfo = fld(AbsUserInfo, RelUserInfo)

  /** Nullable */
  private val _host = fld(AbsHost, RelHost)

  /** `-1` means not present */
  private val _port = {
    val portPart = fld(AbsPort, RelPort)
    if (portPart == null) -1 else Integer.parseInt(portPart)
  }

  /** Nullable */
  private val _path = {
    val useNetPath = fld(AbsAuthority, RelAuthority) != null
    if (useNetPath) {
      val netPath = fld(AbsNetPath, RelNetPath)
      if (netPath == null) "" else netPath
    } else if (_isAbsolute) {
      fld(AbsAbsPath)
    } else {
      val relAbsPath = fld(RelAbsPath)
      if (relAbsPath != null) relAbsPath else fld(RelRelPath)
    }
  }

  /** Nullable */
  private val _query = fld(AbsQuery, RelQuery)

  /** Nullable */
  private val _fragment = fld(Fragment)

  // End of default ctor. Unset helper field
  _fld = null

  def this(scheme: String, ssp: String, fragment: String) =
    this(URI.uriStr(scheme, ssp, fragment))

  def this(scheme: String, userInfo: String, host: String, port: Int,
      path: String, query: String, fragment: String) = {
    this(URI.uriStr(scheme, userInfo, host, port, path, query, fragment))
    parseServerAuthority()
  }

  def this(scheme: String, host: String, path: String, fragment: String) =
    this(scheme, null, host, -1, path, null, fragment)

  def this(scheme: String, authority: String, path: String, query: String,
      fragment: String) = {
    this(URI.uriStr(scheme, authority, path, query, fragment))
    // JavaDoc says to invoke parseServerAuthority() here, but in practice
    // it isn't invoked. This makes sense, since you want to be able
    // to create URIs with registry-based authorities.
    // parseServerAuthority()
  }

  def compareTo(that: URI): Int = {
    import URI.{caseInsensitiveCompare, escapeAwareCompare => cmp}

    def comparePathQueryFragement(): Int = {
      val cmpPath = cmp(this._path, that._path)
      if (cmpPath != 0) {
        cmpPath
      } else {
        val cmpQuery = cmp(this._query, that._query)
        if (cmpQuery != 0) cmpQuery
        else cmp(this._fragment, that._fragment)
      }
    }

    val cmpScheme = caseInsensitiveCompare(this._scheme, that._scheme)
    if (cmpScheme != 0) {
      cmpScheme
    } else {
      // A hierarchical URI is less than an opaque URI
      val cmpIsOpaque = java.lang.Boolean.compare(this.isOpaque(), that.isOpaque())
      if (cmpIsOpaque != 0) {
        cmpIsOpaque
      } else {
        if (this.isOpaque()) {
          val cmpSchemeSpecificPart = cmp(this._schemeSpecificPart, that._schemeSpecificPart)
          if (cmpSchemeSpecificPart != 0) cmpSchemeSpecificPart
          else comparePathQueryFragement()
        } else if (this._host != null && that._host != null) {
          val cmpUserInfo = cmp(this._userInfo, that._userInfo)
          if (cmpUserInfo != 0) {
            cmpUserInfo
          } else {
            val cmpHost = caseInsensitiveCompare(this._host, that._host)
            if (cmpHost != 0) {
              cmpHost
            } else {
              val cmpPort = this._port - that._port // absent as -1 is smaller than valid port numbers
              if (cmpPort != 0) cmpPort
              else comparePathQueryFragement()
            }
          }
        } else {
          val cmpAuthority = cmp(this._authority, that._authority)
          if (cmpAuthority != 0) cmpAuthority
          else comparePathQueryFragement()
        }
      }
    }
  }

  override def equals(that: Any): Boolean = that match {
    case that: URI => this.compareTo(that) == 0
    case _ => false
  }

  def getAuthority(): String = decodeComponent(_authority)
  def getFragment(): String = decodeComponent(_fragment)
  def getHost(): String = _host
  def getPath(): String = decodeComponent(_path)
  def getPort(): Int = _port
  def getQuery(): String = decodeComponent(_query)
  def getRawAuthority(): String = _authority
  def getRawFragment(): String = _fragment
  def getRawPath(): String = _path
  def getRawQuery(): String = _query
  def getRawSchemeSpecificPart(): String = _schemeSpecificPart
  def getRawUserInfo(): String = _userInfo
  def getScheme(): String = _scheme
  def getSchemeSpecificPart(): String = decodeComponent(_schemeSpecificPart)
  def getUserInfo(): String = decodeComponent(_userInfo)

  override def hashCode(): Int = {
    import scala.util.hashing.MurmurHash3._
    import URI.normalizeEscapes

    var acc = URI.uriSeed
    acc = mix(acc, if (_scheme == null) 0 else _scheme.toLowerCase.##) // scheme may not contain escapes
    if (this.isOpaque()) {
      acc = mix(acc, normalizeEscapes(this._schemeSpecificPart).##)
    } else if (this._host != null) {
      acc = mix(acc, normalizeEscapes(this._userInfo).##)
      acc = mix(acc, this._host.toLowerCase.##)
      acc = mix(acc, this._port.##)
    } else {
      acc = mix(acc, normalizeEscapes(this._authority).##)
    }
    acc = mix(acc, normalizeEscapes(this._path).##)
    acc = mix(acc, normalizeEscapes(this._query).##)
    acc = mixLast(acc, normalizeEscapes(this._fragment).##)
    finalizeHash(acc, 3)
  }

  def isAbsolute(): Boolean = _isAbsolute
  def isOpaque(): Boolean = _isOpaque

  def normalize(): URI = if (_isOpaque || _path == null) this else {
    import js.JSStringOps._

    val origPath = _path

    val segments = origPath.jsSplit("/")

    // Step 1: Remove all "." segments
    // Step 2: Remove ".." segments preceded by non ".." segment until no
    // longer applicable

    val inLen = segments.length
    val isAbsPath = inLen != 0 && segments(0) == ""

    // Do not inject the first empty segment into the normalization loop,
    // so that we don't need to special-case it inside.
    val startIdx = if (isAbsPath) 1 else 0
    var inIdx = startIdx
    var outIdx = startIdx

    while (inIdx != inLen) {
      val segment = segments(inIdx)
      inIdx += 1 // do this before the rest of the loop

      if (segment == ".") {
        if (inIdx == inLen) {
          // convert "." segments at end to an empty segment
          // (consider: /a/b/. => /a/b/, not /a/b)
          segments(outIdx) = ""
          outIdx += 1
        } else {
          // remove "." segments, so do not increment outIdx
        }
      } else if (segment == "..") {
        val okToDrop = outIdx != startIdx && {
          val lastSegment = segments(outIdx - 1)
          lastSegment != ".." && lastSegment != ""
        }
        if (okToDrop) {
          if (inIdx == inLen) { // did we reach the end?
            // prevent a ".." segment at end to change a "dir" into a "file"
            // (consider: /a/b/.. => /a/, not /a)
            segments(outIdx - 1) = ""
            // do not increment outIdx
          } else {
            // remove preceding segment (it is not "..")
            outIdx -= 1
          }
        } else {
          // cannot drop
          segments(outIdx) = ".."
          outIdx += 1
        }
      } else if (segment == "" && inIdx != inLen) {
        // remove empty segments not at end of path
        // do not increment outIdx
      } else {
        // keep the segment
        segments(outIdx) = segment
        outIdx += 1
      }
    }

    // Truncate `segments` at `outIdx`
    segments.length = outIdx

    // Step 3: If path is relative and first segment contains ":", prepend "."
    // segment (according to JavaDoc). If the path is absolute, the first
    // segment is "" so the `contains(':')` returns false.
    if (outIdx != 0 && segments(0).contains(":"))
      segments.unshift(".")

    // Now add all the segments from step 1, 2 and 3
    val newPath = segments.join("/")

    // Only create new instance if anything changed
    if (newPath == origPath)
      this
    else
      new URI(getScheme(), getRawAuthority(), newPath, getQuery(), getFragment())
  }

  def parseServerAuthority(): URI = {
    if (_authority != null && _host == null)
      throw new URISyntaxException(origStr, "No Host in URI")
    else this
  }

  def relativize(uri: URI): URI = {
    if (this.isOpaque() || uri.isOpaque() || this._scheme != uri._scheme ||
        URI.escapeAwareCompare(this._authority, uri._authority) != 0) {
      uri
    } else {
      val thisN = this.normalize()
      val uriN = uri.normalize()

      // Strangely, Java doesn't handle escapes here. So we don't
      if (uriN.getRawPath().startsWith(thisN.getRawPath())) {
        val newPath = uriN.getRawPath().substring(thisN.getRawPath().length())

        new URI(scheme = null, authority = null,
            // never produce an abs path if we relativized
            path = if (newPath.startsWith("/")) newPath.substring(1) else newPath,
            query = uri.getQuery(), fragment = uri.getFragment())
      } else uri
    }
  }

  def resolve(str: String): URI = resolve(URI.create(str))

  def resolve(uri: URI): URI = {
    if (uri.isAbsolute() || this.isOpaque()) uri
    else if (uri._scheme == null && uri._authority == null &&
      uri._path == "" && uri._query == null)
      // This is a special case for URIs like: "#foo". This allows to
      // just change the fragment in the current document.
      new URI(
        this.getScheme(),
        this.getRawAuthority(),
        this.getRawPath(),
        this.getRawQuery(),
        uri.getRawFragment())
    else if (uri._authority != null)
      new URI(
        this.getScheme(),
        uri.getRawAuthority(),
        uri.getRawPath(),
        uri.getRawQuery(),
        uri.getRawFragment())
    else if (uri._path.startsWith("/"))
      new URI(
        this.getScheme(),
        this.getRawAuthority(),
        uri.getRawPath(),
        uri.getRawQuery(),
        uri.getRawFragment())
    else {
      val basePath = this._path
      val relPath = uri._path
      val endIdx = basePath.lastIndexOf('/')
      val path =
        if (endIdx == -1) relPath
        else basePath.substring(0, endIdx+1) + relPath
      new URI(
        this.getScheme(),
        this.getAuthority(),
        path,
        uri.getRawQuery(),
        uri.getRawFragment()).normalize()
    }
  }

  def toASCIIString(): String = quoteNonASCII(origStr)

  override def toString(): String = origStr

  // Not implemented:
  // def toURL(): URL

}

object URI {

  def create(str: String): URI = {
    try new URI(str)
    catch {
      case e: URISyntaxException => throw new IllegalArgumentException(e)
    }
  }

  // IPv4address   = 1*digit "." 1*digit "." 1*digit "." 1*digit
  private final val ipv4address = {
    val digit = "(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"
    s"(?:$digit\\.){3}$digit"
  }

  private final val ipv6address = {
    // http://stackoverflow.com/a/17871737/1149944
    val block = "[0-9a-f]{1,4}"
    val lelem = "(?:"+block+":)"
    val relem = "(?::"+block+")"
    val ipv4 = ipv4address

    "(?:" +
    lelem+"{7}"+block+"|"+                 // 1:2:3:4:5:6:7:8
    lelem+"{1,7}:|"+                       // 1::                                        1:2:3:4:5:6:7::
    lelem+"{1,6}"+relem+"|"+               // 1::8                  1:2:3:4:5:6::8       1:2:3:4:5:6::8
    lelem+"{1,5}"+relem+"{1,2}|"+          // 1::7:8                1:2:3:4:5::7:8       1:2:3:4:5::8
    lelem+"{1,4}"+relem+"{1,3}|"+          // 1::6:7:8              1:2:3:4::6:7:8       1:2:3:4::8
    lelem+"{1,3}"+relem+"{1,4}|"+          // 1::5:6:7:8            1:2:3::5:6:7:8       1:2:3::8
    lelem+"{1,2}"+relem+"{1,5}|"+          // 1::4:5:6:7:8          1:2::4:5:6:7:8       1:2::8
    lelem        +relem+"{1,6}|"+          // 1::3:4:5:6:7:8        1::3:4:5:6:7:8       1::8
    ":(?:"+relem+"{1,7}|:)|" +             // ::2:3:4:5:6:7:8       ::2:3:4:5:6:7:8      ::8       ::
    lelem+"{6}"+ipv4+"|"+                  // 1:2:3:4:5:6:10.0.0.1
    lelem+"{1,5}:"+ipv4+"|"+               // 1::10.0.0.1           1:2:3:4:5::10.0.0.1
    lelem+"{1,4}"+relem+":"+ipv4+"|"+      // 1::6:10.0.0.1         1:2:3:4::6:10.0.0.1
    lelem+"{1,3}"+relem+"{1,2}:"+ipv4+"|"+ // 1::5:6:10.0.0.1       1:2:3::5:6:10.0.0.1  1:2:3::6:10.0.0.1
    lelem+"{1,2}"+relem+"{1,3}:"+ipv4+"|"+ // 1::4:5:6:10.0.0.1     1:2::4:5:6:10.0.0.1  1:2::6:10.0.0.1
    lelem        +relem+"{1,4}:"+ipv4+"|"+ // 1::3:4:5:6:10.0.0.1   1::3:4:5:6:10.0.0.1  1::6:10.0.0.1
    "::"+lelem+"{1,5}"+ipv4+               // ::2:3:4:5:10.0.0.1    ::5:10.0.0.1         ::10.0.0.1
    ")(?:%[0-9a-z]+)?"

    // scalastyle:off line.size.limit

    // This was part of the original regex, but is too specific to
    // IPv6 details.
    // fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|     # fe80::7:8%eth0   fe80::7:8%1     (link-local IPv6 addresses with zone index)
    // ::(ffff(:0{1,4}){0,1}:){0,1}
    // ((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]).){3,3}
    // (25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|          # ::255.255.255.255   ::ffff:255.255.255.255  ::ffff:0:255.255.255.255  (IPv4-mapped IPv6 addresses and IPv4-translated addresses)
    // ([0-9a-fA-F]{1,4}:){1,4}:
    // ((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]).){3,3}
    // (25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])           # 2001:db8:3:4::192.0.2.33  64:ff9b::192.0.2.33 (IPv4-Embedded IPv6 Address)

    // scalastyle:on line.size.limit
  }

  private val ipv6Re = new RegExp("^"+ipv6address+"$", "i")

  // URI syntax parser. Based on RFC2396, RFC2732 and adaptations according to
  // JavaDoc.
  // - http://www.ietf.org/rfc/rfc2396.txt (see Appendix A for complete syntax)
  // - http://www.ietf.org/rfc/rfc2732.txt

  private val uriRe = {
    // We don't use any interpolators here to allow for constant folding

    ///////////////////
    ////  Helpers  ////
    ///////////////////

    // Inlined definitions
    // reserved      = ";" | "/" | "?" | ":" | "@" | "&" | "=" | "+" |
    //                  "$" | "," | "[" | "]" ; last two added by RFC2732
    // unreserved    = alphanum | mark
    // mark          = "-" | "_" | "." | "!" | "~" | "*" | "'" |
    //                 "(" | ")"

    // escaped       = "%" hex hex
    val escaped = "%[a-f0-9]{2}"

    // other         = anything but: ASCII + control chars + no-break-space
    //                 SPACE_SEPARATOR + LINE_SEPARATOR + PARAGRAPH_SEPARATOR
    // any use of this category is in deviation to RFC 2396, which is ASCII only
    val other =
      "[^\u0000-\u00a0\u1680\u2000-\u200a\u202f\u205f\u3000\u2028\u2029]"

    // uric          = reserved | unreserved | escaped | other
    val uric = "(?:[;/?:@&=+$,\\[\\]a-z0-9-_.!~*'()]|"+escaped+"|"+other+")"

    // pchar         = unreserved | escaped | other |
    //                 ":" | "@" | "&" | "=" | "+" | "$" | ","
    val pchar = "(?:[a-z0-9-_.!~*'():@&=+$,]|"+escaped+"|"+other+")"

    ///////////////////
    ////  Server   ////
    ///////////////////

    // domainlabel   = alphanum | alphanum *( alphanum | "-" ) alphanum
    val domainlabel = "(?:[a-z0-9]|[a-z0-9][a-z0-9-]*[a-z0-9])"

    // toplabel      = alpha | alpha *( alphanum | "-" ) alphanum
    val toplabel = "(?:[a-z]|[a-z][a-z0-9-]*[a-z0-9])"

    // hostname      = *( domainlabel "." ) toplabel [ "." ]
    val hostname = "(?:"+domainlabel+"\\.)*"+toplabel+"\\.?"

    // IPv6reference = "[" IPv6address "]"
    val ipv6reference = "\\[(?:"+ipv6address+")\\]"

    // host          = hostname | IPv4address | IPv6reference
    //               ; IPv6reference added by RFC2732
    val host = "("+hostname+"|"+ipv4address+"|"+ipv6reference+")" /*CAPT*/

    // Inlined definition
    // port          = *digit

    // hostport      = host [ ":" port ]
    val hostport = host+"(?::([0-9]*))?" /*CAPT*/

    // userinfo      = *( unreserved | escaped | other |
    //                    ";" | ":" | "&" | "=" | "+" | "$" | "," )
    val userinfo = "(?:[a-z0-9-_.!~*'();:&=+$,]|"+escaped+"|"+other+")*"

    // server        = [ [ userinfo "@" ] hostport ]
    val server = "(?:(?:("+userinfo+")@)?"+hostport+")?" /*CAPT*/

    ///////////////////
    //// Authority ////
    ///////////////////

    // reg_name      = 1*( unreserved | escaped | other | "$" | "," |
    //                     ";" | ":" | "@" | "&" | "=" | "+" )
    val reg_name = "(?:[a-z0-9-_.!~*'()$,;:@&=+]|"+escaped+"|"+other+")+"

    // authority     = server | reg_name
    val authority = server+"|"+reg_name

    ///////////////////
    ////   Paths   ////
    ///////////////////

    // Inlined definitions
    // param         = *pchar

    // segment       = *pchar *( ";" param )
    val segment = pchar+"*(?:;"+pchar+"*)*"

    // path_segments = segment *( "/" segment )
    val path_segments = segment+"(?:/"+segment+")*"

    // abs_path      = "/"  path_segments
    val abs_path = "/"+path_segments

    // net_path      = "//" authority [ abs_path ]
    val net_path = "//("+authority+")("+abs_path+")?" /*2CAPT*/

    // Inlined definition
    // Deviation from RCF2396 according to JavaDoc: Allow empty rel_segment
    // and hence empty rel_path
    // rel_segment   = 1*( unreserved | escaped |
    //                     ";" | "@" | "&" | "=" | "+" | "$" | "," )

    // rel_path      = rel_segment [ abs_path ]
    val rel_path = "(?:[a-z0-9-_.!~*'();@&=+$,]|"+escaped+")*(?:"+abs_path+")?"

    ///////////////////
    /// Query/Frag  ///
    ///////////////////

    // query         = *uric
    val query = "("+uric+"*)" /*CAPT*/
    // fragment      = *uric
    val fragment = "("+uric+"*)" /*CAPT*/

    ///////////////////
    ///    Parts    ///
    ///////////////////

    // hier_part     = ( net_path | abs_path ) [ "?" query ]
    val hier_part = "(?:"+net_path+"|("+abs_path+"))(?:\\?"+query+")?" /*CAPT*/

    // Inlined definition
    // uric_no_slash = unreserved | escaped | ";" | "?" | ":" | "@" |
    //                 "&" | "=" | "+" | "$" | ","

    // opaque_part   = uric_no_slash *uric
    val opaque_part = "(?:[a-z0-9-_.!~*'();?:@&=+$,]|"+escaped+")"+uric+"*"

    ///////////////////
    ///    URIs     ///
    ///////////////////

    // scheme        = alpha *( alpha | digit | "+" | "-" | "." )
    val scheme = "([a-z][a-z0-9+-.]*)" /*CAPT*/

    // absoluteURI   = scheme ":" ( hier_part | opaque_part )
    val absoluteURI = scheme+":(?:("+hier_part+")|("+opaque_part+"))" /*2CAPT*/

    // relativeURI   = ( net_path | abs_path | rel_path ) [ "?" query ]
    val relativeURI = /*3CAPT*/
      "((?:"+net_path+"|("+abs_path+")|("+rel_path+"))(?:\\?"+query+")?)"

    // URI-reference = [ absoluteURI | relativeURI ] [ "#" fragment ]
    val uriRef = "^(?:"+absoluteURI+"|"+relativeURI+")(?:#"+fragment+")?$"

    new RegExp(uriRef, "i")
  }

  private object Fields {
    final val AbsScheme = 1
    final val AbsHierPart = AbsScheme+1
    final val AbsAuthority = AbsHierPart+1
    final val AbsUserInfo = AbsAuthority+1
    final val AbsHost = AbsUserInfo+1
    final val AbsPort = AbsHost+1
    final val AbsNetPath = AbsPort+1 // abs_path part only
    final val AbsAbsPath = AbsNetPath+1
    final val AbsQuery = AbsAbsPath+1
    final val AbsOpaquePart = AbsQuery+1
    final val RelSchemeSpecificPart = AbsOpaquePart+1 // Everything but the fragment
    final val RelAuthority = RelSchemeSpecificPart+1
    final val RelUserInfo = RelAuthority+1
    final val RelHost = RelUserInfo+1
    final val RelPort = RelHost+1
    final val RelNetPath = RelPort+1 // abs_path part only
    final val RelAbsPath = RelNetPath+1
    final val RelRelPath = RelAbsPath+1
    final val RelQuery = RelRelPath+1
    final val Fragment = RelQuery+1
  }

  // Helpers for constructors

  private def uriStr(scheme: String, ssp: String, fragment: String): String = {
    var resStr = ""

    if (scheme != null)
      resStr += scheme + ":"

    if (ssp != null)
      resStr += quoteIllegal(ssp)

    if (fragment != null)
      resStr += "#" + quoteIllegal(fragment)

    resStr
  }

  private def uriStr(scheme: String, userInfo: String, host: String, port: Int,
      path: String, query: String, fragment: String): String = {
    var resStr = ""

    if (scheme != null)
      resStr += scheme + ":"

    if (userInfo != null || host != null || port != -1)
      resStr += "//"

    if (userInfo != null)
      resStr += quoteUserInfo(userInfo) + "@"

    if (host != null) {
      if (URI.ipv6Re.test(host))
        resStr += "[" + host + "]"
      else
        resStr += host
    }

    if (port != -1)
      resStr += ":" + port

    if (path != null)
      resStr += quotePath(path)

    if (query != null)
      resStr += "?" + quoteIllegal(query)

    if (fragment != null)
      resStr += "#" + quoteIllegal(fragment)

    resStr
  }

  private def uriStr(scheme: String, authority: String, path: String,
      query: String, fragment: String) = {
    var resStr = ""

    if (scheme != null)
      resStr += scheme + ":"

    if (authority != null)
      resStr += "//" + quoteAuthority(authority)

    if (path != null)
      resStr += quotePath(path)

    if (query != null)
      resStr += "?" + quoteIllegal(query)

    if (fragment != null)
      resStr += "#" + quoteIllegal(fragment)

    resStr
  }

  // Quote helpers

  private def decodeComponent(str: String): String = {
    def containsNoEncodedComponent(): Boolean = {
      // scalastyle:off return
      var i = 0
      while (i != str.length) {
        if (str.charAt(i) == '%')
          return false
        i += 1
      }
      true
      // scalastyle:on return
    }

    // Fast-track, if null or no encoded components
    if (str == null || containsNoEncodedComponent()) {
      str
    } else {
      val inBuf = CharBuffer.wrap(str)
      val outBuf = CharBuffer.allocate(inBuf.capacity())
      val byteBuf = ByteBuffer.allocate(64)
      var decoding = false
      val decoder = StandardCharsets.UTF_8.newDecoder()
        .onMalformedInput(CodingErrorAction.REPLACE)
        .onUnmappableCharacter(CodingErrorAction.REPLACE)

      def decode(endOfInput: Boolean) = {
        byteBuf.flip()
        decoder.decode(byteBuf, outBuf, endOfInput)
        if (endOfInput) {
          decoder.reset()
          byteBuf.clear()
          decoding = false
        } else {
          byteBuf.compact()
        }
      }

      while (inBuf.hasRemaining()) {
        inBuf.get() match {
          case '%' =>
            if (!byteBuf.hasRemaining())
              decode(false)

            // get two chars - they must exist, otherwise the URI would not have
            // passed syntax checking
            val hexStr = inBuf.get().toString + inBuf.get().toString
            val v = Integer.parseInt(hexStr, 16)
            byteBuf.put(v.toByte)
            decoding = true

          case c =>
            if (decoding)
              decode(true)
            outBuf.put(c)
        }
      }

      if (decoding)
        decode(true)

      outBuf.flip()
      outBuf.toString
    }
  }

  private val quoteStr: js.Function1[String, String] = { (str: String) =>
    val buf = StandardCharsets.UTF_8.encode(str)

    var res = ""
    while (buf.hasRemaining()) {
      val c = buf.get() & 0xff
      res += (if (c <= 0xf) "%0" else "%") + Integer.toHexString(c).toUpperCase
    }

    res
  }

  /** matches any character not in unreserved, punct, escaped or other */
  private val userInfoQuoteRe = new RegExp(
    // !other = [\u0000-\u00a0\u1680\u2000-\u200a\u202f\u205f\u3000\u2028\u2029]
    // Char class is: [:!other:^a-z0-9-_.!~*'(),;:$&+=%]
    "[\u0000- \"#/<>?@\\[-\\^`{-}" +
    "\u007f-\u00a0\u1680\u2000-\u200a\u202f\u205f\u3000\u2028\u2029]|" +
    "%(?![0-9a-f]{2})", "ig")

  /** Quote any character not in unreserved, punct, escaped or other */
  private def quoteUserInfo(str: String) = {
    import js.JSStringOps._
    str.jsReplace(userInfoQuoteRe, quoteStr)
  }

  /** matches any character not in unreserved, punct, escaped, other or equal
   *  to '/' or '@'
   */
  private val pathQuoteRe = new RegExp(
    // !other = [\u0000-\u00a0\u1680\u2000-\u200a\u202f\u205f\u3000\u2028\u2029]
    // Char class is: [:!other:^a-z0-9-_.!~*'(),;:$&+=%@/]
    "[\u0000- \"#<>?\\[-\\^`{-}" +
    "\u007f-\u00a0\u1680\u2000-\u200a\u202f\u205f\u3000\u2028\u2029]|" +
    "%(?![0-9a-f]{2})", "ig")

  /** Quote any character not in unreserved, punct, escaped, other or equal
   *  to '/' or '@'
   */
  private def quotePath(str: String) = {
    import js.JSStringOps._
    str.jsReplace(pathQuoteRe, quoteStr)
  }

  /** matches any character not in unreserved, punct, escaped, other or equal
   *  to '@', '[' or ']'
   *  The last two are different to how JavaDoc specifies, but hopefully yield
   *  the same behavior. (We shouldn't escape [], since they may occur
   *  in IPv6 addresses, but technically speaking they are in reserved
   *  due to RFC2732).
   */
  private val authorityQuoteRe = new RegExp(
    // !other = [\u0000-\u00a0\u1680\u2000-\u200a\u202f\u205f\u3000\u2028\u2029]
    // Char class is: [:!other:^a-z0-9-_.!~*'(),;:$&+=%@\[\]]
    "[\u0000- \"#/<>?\\^`{-}" +
    "\u007f-\u00a0\u1680\u2000-\u200a\u202f\u205f\u3000\u2028\u2029]|" +
    "%(?![0-9a-f]{2})", "ig")

  /** Quote any character not in unreserved, punct, escaped, other or equal
   *  to '@'
   */
  private def quoteAuthority(str: String) = {
    import js.JSStringOps._
    str.jsReplace(authorityQuoteRe, quoteStr)
  }

  /** matches any character not in unreserved, reserved, escaped or other */
  private val illegalQuoteRe = new RegExp(
    // !other = [\u0000-\u00a0\u1680\u2000-\u200a\u202f\u205f\u3000\u2028\u2029]
    // Char class is: [:!other:^a-z0-9-_.!~*'(),;:$&+=?/\\[\\]%]
    "[\u0000- \"#<>@\\^`{-}" +
    "\u007f-\u00a0\u1680\u2000-\u200a\u202f\u205f\u3000\u2028\u2029]|" +
    "%(?![0-9a-f]{2})", "ig")

  /** Quote any character not in unreserved, reserved, escaped or other */
  private def quoteIllegal(str: String) = {
    import js.JSStringOps._
    str.jsReplace(illegalQuoteRe, quoteStr)
  }

  /** matches characters not in ASCII
   *
   *  Note: It is important that the match is maximal, since we might encounter
   *  surrogates that need to be encoded in one shot.
   */
  private val nonASCIIQuoteRe = new RegExp("[^\u0000-\u007F]+", "g")

  private def quoteNonASCII(str: String) = {
    import js.JSStringOps._
    str.jsReplace(nonASCIIQuoteRe, quoteStr)
  }

  /** Case-insensitive comparison that accepts `null` values.
   *
   *  `null` is considered smaller than any other value.
   */
  private def caseInsensitiveCompare(x: String, y: String): Int = {
    if (x == null)
      if (y == null) 0 else -1
    else
      if (y == null) 1 else x.compareToIgnoreCase(y)
  }

  /** Case-sensitive comparison that is case-insensitive inside URI
   *  escapes. Will compare `a%A0` and `a%a0` as equal, but `a%A0` and
   *  `A%A0` as different.
   *
   *  Accepts `null` arguments. `null` is considered smaller than any other
   *  value.
   */
  private def escapeAwareCompare(x: String, y: String): Int = {
    @tailrec
    def loop(i: Int): Int = {
      if (i >= x.length || i >= y.length)
        x.length - y.length
      else {
        val diff = x.charAt(i) - y.charAt(i)
        if (diff != 0) diff
        else if (x.charAt(i) == '%') {
          // we need to do a CI compare for the next two characters
          if (i + 2 >= x.length || i + 2 >= y.length)
            throw new AssertionError("Invalid escape in URI")
          val cmp =
            x.substring(i+1, i+3).compareToIgnoreCase(y.substring(i+1, i+3))
          if (cmp != 0) cmp
          else loop(i+3)
        } else loop(i+1)
      }
    }

    if (x == null)
      if (y == null) 0 else -1
    else
      if (y == null) 1 else loop(0)
  }

  /** Upper-cases all URI escape sequences in the nullable `str`. Used for hashing */
  private def normalizeEscapes(str: String): String = {
    if (str == null) {
      null
    } else {
      var i = 0
      var res = ""
      while (i < str.length) {
        if (str.charAt(i) == '%') {
          if (i + 2 >= str.length)
            throw new AssertionError("Invalid escape in URI")
          res += str.substring(i, i+3).toUpperCase()
          i += 3
        } else {
          res += str.substring(i, i+1)
          i += 1
        }
      }
      res
    }
  }

  private final val uriSeed = 53722356

}
