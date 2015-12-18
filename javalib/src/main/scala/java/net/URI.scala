package java.net

import scala.scalajs.js.RegExp
import scala.scalajs.js

import scala.scalajs.niocharset.StandardCharsets

import scala.annotation.tailrec

import java.nio._
import java.nio.charset.CodingErrorAction

final class URI(origStr: String) extends Serializable with Comparable[URI] {

  import URI.Fields._
  import URI.decodeComponent
  import URI.quoteNonASCII

  /** The fields matched in the regular expression.
   *
   *  This is a local val for the primary constructor. It is a val,
   *  since we'll set it to null after initializing all fields.
   */
  private[this] var _fld = Option(URI.uriRe.exec(origStr)).getOrElse {
    throw new URISyntaxException(origStr, "Malformed URI")
  }

  private val _isAbsolute = fld(AbsScheme).isDefined
  private val _isOpaque = fld(AbsOpaquePart).isDefined

  @inline private def fld(idx: Int): js.UndefOr[String] = _fld(idx)

  @inline private def fld(absIdx: Int, relIdx: Int): js.UndefOr[String] =
    if (_isAbsolute) _fld(absIdx) else _fld(relIdx)

  private val _scheme = fld(AbsScheme)

  private val _schemeSpecificPart = {
    if (!_isAbsolute) fld(RelSchemeSpecificPart)
    else if (_isOpaque) fld(AbsOpaquePart)
    else fld(AbsHierPart)
  }.get

  private val _authority = fld(AbsAuthority, RelAuthority).filter(_ != "")
  private val _userInfo = fld(AbsUserInfo, RelUserInfo)
  private val _host = fld(AbsHost, RelHost)
  private val _port = fld(AbsPort, RelPort).fold(-1)(_.toInt)

  private val _path = {
    val useNetPath = fld(AbsAuthority, RelAuthority).isDefined
    if (useNetPath)
      fld(AbsNetPath, RelNetPath) orElse ""
    else if (_isAbsolute)
      fld(AbsAbsPath)
    else
      fld(RelAbsPath) orElse fld(RelRelPath)
  }

  private val _query = fld(AbsQuery, RelQuery)
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

  /** Compare this URI to another URI while supplying a comparator
   *
   *  This helper is required to account for the semantic differences
   *  between [[compareTo]] and [[equals]]. ([[equals]] does treat
   *  URI escapes specially: they are never case-sensitive).
   */
  @inline
  private def internalCompare(that: URI)(cmp: (String, String) => Int): Int = {
    @inline def cmpOpt(x: js.UndefOr[String], y: js.UndefOr[String]): Int = {
      if (x == y) 0
      // Undefined components are considered less than defined components
      else x.fold(-1)(s1 => y.fold(1)(s2 => cmp(s1, s2)))
    }

    if (this._scheme != that._scheme)
      this._scheme.fold(-1)(s1 => that._scheme.fold(1)(s1.compareToIgnoreCase))
    else if (this._isOpaque != that._isOpaque)
      // A hierarchical URI is less than an opaque URI
      if (this._isOpaque) 1 else -1
    else if (_isOpaque) {
      val ssp = cmp(this._schemeSpecificPart, that._schemeSpecificPart)
      if (ssp != 0) ssp
      else cmpOpt(this._fragment, that._fragment)
    } else if (this._authority != that._authority) {
      if (this._host.isDefined && that._host.isDefined) {
        val ui = cmpOpt(this._userInfo, that._userInfo)
        if (ui != 0) ui
        else {
          val hst = this._host.get.compareToIgnoreCase(that._host.get)
          if (hst != 0) hst
          else if (this._port == that._port) 0
          else if (this._port == -1) -1
          else if (that._port == -1)  1
          else this._port - that._port
        }
      } else
        cmpOpt(this._authority, that._authority)
    } else if (this._path != that._path)
      cmpOpt(this._path, that._path)
    else if (this._query != that._query)
      cmpOpt(this._query, that._query)
    else
      cmpOpt(this._fragment, that._fragment)
  }

  def compareTo(that: URI): Int = internalCompare(that)(_.compareTo(_))

  override def equals(that: Any): Boolean = that match {
    case that: URI => internalCompare(that)(URI.escapeAwareCompare) == 0
    case _ => false
  }

  def getAuthority(): String = _authority.map(decodeComponent).orNull
  def getFragment(): String = _fragment.map(decodeComponent).orNull
  def getHost(): String = _host.orNull
  def getPath(): String = _path.map(decodeComponent).orNull
  def getPort(): Int = _port
  def getQuery(): String = _query.map(decodeComponent).orNull
  def getRawAuthority(): String = _authority.orNull
  def getRawFragment(): String = _fragment.orNull
  def getRawPath(): String = _path.orNull
  def getRawQuery(): String = _query.orNull
  def getRawSchemeSpecificPart(): String = _schemeSpecificPart
  def getRawUserInfo(): String = _userInfo.orNull
  def getScheme(): String = _scheme.orNull
  def getSchemeSpecificPart(): String = decodeComponent(_schemeSpecificPart)
  def getUserInfo(): String = _userInfo.map(decodeComponent).orNull

  override def hashCode(): Int = {
    import scala.util.hashing.MurmurHash3._
    import URI.normalizeEscapes

    var acc = URI.uriSeed
    acc = mix(acc, _scheme.##) // scheme may not contain escapes
    acc = mix(acc, normalizeEscapes(_schemeSpecificPart).##)
    acc = mixLast(acc, _fragment.map(normalizeEscapes).##)

    finalizeHash(acc, 3)
  }

  def isAbsolute(): Boolean = _isAbsolute
  def isOpaque(): Boolean = _isOpaque

  def normalize(): URI = if (_isOpaque || _path.isEmpty) this else {
    val origPath = _path.get

    // Step 1: Remove all "." segments
    // Step 2: Remove ".." segments preceeded by non ".." segment until no
    // longer applicable

    /** Checks whether a successive ".." may drop the head of a
     *  reversed segment list.
     */
    def okToDropFrom(resRev: List[String]) =
      resRev.nonEmpty && resRev.head != ".." && resRev.head != ""

    @tailrec
    def loop(in: List[String], resRev: List[String]): List[String] = in match {
      case "." :: Nil =>
        // convert "." segments at end to an empty segment
        // (consider: /a/b/. => /a/b/, not /a/b)
        loop(Nil, "" :: resRev)
      case ".." :: Nil if okToDropFrom(resRev) =>
        // prevent a ".." segment at end to change a "dir" into a "file"
        // (consider: /a/b/.. => /a/, not /a)
        loop(Nil, "" :: resRev.tail)
      case "." :: xs =>
        // remove "." segments
        loop(xs, resRev)
      case "" :: xs if xs.nonEmpty =>
        // remove empty segments not at end of path
        loop(xs, resRev)
      case ".." :: xs if okToDropFrom(resRev) =>
        // Remove preceeding non-".." segment
        loop(xs, resRev.tail)
      case x :: xs =>
        loop(xs, x :: resRev)
      case Nil =>
        resRev.reverse
    }

    // Split into segments. -1 since we want empty trailing ones
    val segments0 = origPath.split("/", -1).toList
    val isAbsPath = segments0.nonEmpty && segments0.head == ""
    // Don't inject first empty segment into normalization loop, so we
    // won't need to special case it.
    val segments1 = if (isAbsPath) segments0.tail else segments0
    val segments2 = loop(segments1, Nil)

    // Step 3: If path is relative and first segment contains ":", prepend "."
    // segment (according to JavaDoc). If it is absolute, add empty
    // segment again to have leading "/".
    val segments3 = {
      if (isAbsPath)
        "" :: segments2
      else if (segments2.nonEmpty && segments2.head.contains(':'))
        "." :: segments2
      else segments2
    }

    val newPath = segments3.mkString("/")

    // Only create new instance if anything changed
    if (newPath == origPath)
      this
    else
      new URI(getScheme(), getRawAuthority(), newPath, getQuery(), getFragment())
  }

  def parseServerAuthority(): URI = {
    if (_authority.nonEmpty && _host.isEmpty)
      throw new URISyntaxException(origStr, "No Host in URI")
    else this
  }

  def relativize(uri: URI): URI = {
    def authoritiesEqual = this._authority.fold(uri._authority.isEmpty) { a1 =>
      uri._authority.fold(false)(a2 => URI.escapeAwareCompare(a1, a2) == 0)
    }

    if (this.isOpaque || uri.isOpaque ||
      this._scheme != uri._scheme || !authoritiesEqual) uri
    else {
      val thisN = this.normalize()
      val uriN = uri.normalize()

      // Strangely, Java doesn't handle escapes here. So we don't
      if (uriN.getRawPath().startsWith(thisN.getRawPath())) {
        val newPath = uriN.getRawPath().stripPrefix(thisN.getRawPath())

        new URI(scheme = null, authority = null,
          // never produce an abs path if we relativized
          path = newPath.stripPrefix("/"),
          query = uri.getQuery(), fragment = uri.getFragment())
      } else uri
    }
  }

  def resolve(str: String): URI = resolve(URI.create(str))

  def resolve(uri: URI): URI = {
    if (uri.isAbsolute() || this.isOpaque()) uri
    else if (uri._scheme.isEmpty && uri._authority.isEmpty &&
      uri._path.get == "" && uri._query.isEmpty)
      // This is a special case for URIs like: "#foo". This allows to
      // just change the fragment in the current document.
      new URI(
        this.getScheme(),
        this.getRawAuthority(),
        this.getRawPath(),
        this.getRawQuery(),
        uri.getRawFragment())
    else if (uri._authority.isDefined)
      new URI(
        this.getScheme(),
        uri.getRawAuthority(),
        uri.getRawPath(),
        uri.getRawQuery(),
        uri.getRawFragment())
    else if (uri._path.get.startsWith("/"))
      new URI(
        this.getScheme(),
        this.getRawAuthority(),
        uri.getRawPath(),
        uri.getRawQuery(),
        uri.getRawFragment())
    else {
      val basePath = this._path.get
      val relPath = uri._path.get
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
  private final val ipv4address = "[0-9]{1,3}(?:\\.[0-9]{1,3}){3}"

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
    // Fast-track, if no encoded components
    if (str.forall(_ != '%')) str
    else {
      val inBuf = CharBuffer.wrap(str)
      val outBuf = CharBuffer.allocate(inBuf.capacity)
      val byteBuf = ByteBuffer.allocate(64)
      var decoding = false
      val decoder = StandardCharsets.UTF_8.newDecoder
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

      while (inBuf.hasRemaining) {
        inBuf.get() match {
          case '%' =>
            if (!byteBuf.hasRemaining)
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
    while (buf.hasRemaining) {
      val c = buf.get & 0xff
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

  /** Case-sensitive comparison that is case-insensitive inside URI
   *  escapes. Will compare `a%A0` and `a%a0` as equal, but `a%A0` and
   *  `A%A0` as different.
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
          assert(x.length > i + 2, "Invalid escape in URI")
          assert(y.length > i + 2, "Invalid escape in URI")
          val cmp =
            x.substring(i+1, i+3).compareToIgnoreCase(y.substring(i+1, i+3))
          if (cmp != 0) cmp
          else loop(i+3)
        } else loop(i+1)
      }
    }

    loop(0)
  }

  /** Upper-cases all URI escape sequences in `str`. Used for hashing */
  private def normalizeEscapes(str: String): String = {
    var i = 0
    var res = ""
    while (i < str.length) {
      if (str.charAt(i) == '%') {
        assert(str.length > i + 2, "Invalid escape in URI")
        res += str.substring(i, i+3).toUpperCase()
        i += 3
      } else {
        res += str.substring(i, i+1)
        i += 1
      }
    }

    res
  }

  private final val uriSeed = 53722356

}
