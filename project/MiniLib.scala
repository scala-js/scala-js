package build

object MiniLib {
  val Whitelist = {
    val inJavaLang = List(
        "Object",
        "Class",

        "CharSequence",
        "Cloneable",
        "Comparable",
        "Number",

        "Void",
        "Boolean",
        "Character",
        "Byte",
        "Short",
        "Integer",
        "Long",
        "Float",
        "Double",
        "String",

        "FloatingPointBits",

        "Throwable",
        "StackTrace",
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
        "StringIndexOutOfBoundsException"
    ).map("java/lang/" + _)

    val inJavaIO = List(
        "Serializable"
    ).map("java/io/" + _)

    val inJavaLangConstant = List(
        "Constable",
        "ConstantDesc"
    ).map("java/lang/constant/" + _)

    val allBaseNames = inJavaLang ::: inJavaIO ::: inJavaLangConstant

    allBaseNames.flatMap(name => List(name + ".sjsir", name + "$.sjsir")).toSet
  }
}
