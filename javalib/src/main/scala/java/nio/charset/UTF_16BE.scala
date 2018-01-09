package java.nio.charset

private[charset] object UTF_16BE extends UTF_16_Common(
    "UTF-16BE", Array(
    "X-UTF-16BE", "UTF_16BE", "ISO-10646-UCS-2", "UnicodeBigUnmarked"),
    endianness = UTF_16_Common.BigEndian)
