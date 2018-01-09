package java.nio.charset

private[charset] object UTF_16 extends UTF_16_Common(
    "UTF-16", Array(
    "utf16", "UTF_16", "UnicodeBig", "unicode"),
    endianness = UTF_16_Common.AutoEndian)
