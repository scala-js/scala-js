package build

object MiniLib {
  val Whitelist = {
    val inJavaLang = List(
        "Object",
        // "Class" is overridden in minilib/
        // "System" is overridden in minilib/

        "CharSequence",
        "Cloneable",
        "Comparable",
        "Number",

        "Boolean",
        "Character",
        "Byte",
        "Short",
        "Integer",
        "Long",
        "Float",
        "Double",
        "String",

        // "FloatingPointBits" is overridden in minilib/

        // "Throwable" is overridden in minilib/
        "Error",
        "VirtualMachineError",
        "Exception",
        "RuntimeException",
        "ArithmeticException",
        "ArrayIndexOutOfBoundsException",
        "ArrayStoreException",
        "ClassCastException",
        "CloneNotSupportedException",
        "IndexOutOfBoundsException",
        "NullPointerException",
        "StringIndexOutOfBoundsException"
    ).map("java/lang/" + _)

    val inJavaIO = List(
        "Serializable"
    ).map("java/io/" + _)

    /* TODO Unfortunately, when a class extends java.io.Serializable, scalac
     * forces its companion object to extend scala.Serializable (ugh!), which
     * means that things like java.lang.Integer$ depend on scala.Serializable.
     * However, as far as the linker is concerned, it shouldn't need, so it
     * would be nice to get rid of this dependency.
     */
    val inScala = List(
        "Serializable"
    ).map("scala/" + _)

    // TODO We should find a way to get rid of this one
    val inScalaRuntime = List(
        "BoxedUnit"
    ).map("scala/runtime/" + _)

    /* TODO Could we put UndefinedBehaviorError in a neutral namespace?
     * RuntimeLong should probably be part of the linker itself, as a resource.
     */
    val inScalaJSRuntime = List(
        "UndefinedBehaviorError",
        "RuntimeLong",
        "RuntimeLong$Utils"
    ).map("scala/scalajs/runtime/" + _)

    val allBaseNames =
      inJavaLang ::: inJavaIO ::: inScala ::: inScalaRuntime ::: inScalaJSRuntime

    allBaseNames.flatMap(name => List(name + ".sjsir", name + "$.sjsir")).toSet
  }
}
