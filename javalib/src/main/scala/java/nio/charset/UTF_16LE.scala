package java.nio.charset

private[charset] object UTF_16LE extends UTF_16_Common(
    "UTF-16LE", Array(
    "UnicodeLittleUnmarked", "UTF_16LE", "X-UTF-16LE"),
    endianness = UTF_16_Common.LittleEndian)
