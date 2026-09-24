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

package org.scalajs.nscplugin.test

import org.scalajs.nscplugin.test.util._

import org.junit.Test

class WasmInteropTest extends DirectTest with TestHelpers {

  override def preamble: String =
    """
    import scala.scalajs.js
    import scala.scalajs.wasm
    import scala.scalajs.wasm.annotation._
    """

  @Test def okWasmInteropDefs(): Unit = {
    """
    object Types {
      type Bytes = Array[Byte]
      type Ints = Array[Int]
      type Id[A] = A
      type Arr[A] = Array[A]
    }

    object Imports {
      @WasmImport("env", "imported")
      def imported(
          i: Int, l: Long, f: Float, d: Double,
          bytes: Array[Byte], shorts: Array[Short],
          ints: Array[Int], longs: Array[Long],
          floats: Array[Float], doubles: Array[Double],
          aBytes: Types.Bytes, aInts: Types.Id[Types.Ints],
          aLongs: Types.Arr[Long]
      ): Types.Arr[Double] = wasm.native
    }

    object Exports {
      @WasmExport("exported")
      def exported(
          i: Int, l: Long, f: Float, d: Double,
          bytes: Array[Byte], shorts: Array[Short],
          ints: Array[Int], longs: Array[Long],
          floats: Array[Float], doubles: Array[Double],
          aBytes: Types.Bytes, aInts: Types.Id[Types.Ints],
          aLongs: Types.Arr[Long]
      ): Unit = ???
    }
    """.hasNoWarns()
  }

  @Test def wasmInteropNonLiteralArguments(): Unit = {
    val nonLiteral =
      """
      object A {
        val module = "env"
        val function = "f"

        @WasmImport(module, "f")
        def imported1(): Unit = wasm.native

        @WasmImport("env", function)
        def imported2(): Unit = wasm.native
      }
      """

    nonLiteral.containsErrors(
        "The arguments to @WasmImport must be literal strings")

    """
    object A {
      val exportName = "f"

      @WasmExport(exportName)
      def exported(): Unit = ???
    }
    """.containsErrors("The argument to @WasmExport must be a literal string")

    // \uD800 is an unpaired high surrogate, which is not valid UTF-16.
    val invalidUTF16Escape = "\\" + "uD800"
    val invalidUTF16 = {
      s"""
      object InvalidUTF16 {
        @WasmImport("$invalidUTF16Escape", "f")
        def imported1(): Unit = wasm.native

        @WasmImport("env", "$invalidUTF16Escape")
        def imported2(): Unit = wasm.native

        @WasmExport("$invalidUTF16Escape")
        def exported(): Unit = ???
      }
      """
    }
    invalidUTF16.containsErrors(
        "The arguments to @WasmImport must be valid UTF-16 strings")
    invalidUTF16.containsErrors(
        "The argument to @WasmExport must be a valid UTF-16 string")
  }

  @Test def wasmInteropOnMethods(): Unit = {
    val constructor =
      """
      class ImportConstructor @WasmImport("env", "ctor") ()
      class ExportConstructor @WasmExport("ctor") ()
      """
    constructor.containsErrors("@WasmImport can only be used on methods")
    constructor.containsErrors("@WasmExport can only be used on methods")

    val nonMethod =
      """
      object A {
        @WasmImport("env", "x")
        val imported: Int = 1

        @WasmExport("x")
        val exported: Int = 1
      }
      """
    nonMethod.containsErrors("@WasmImport can only be used on methods")
    nonMethod.containsErrors("@WasmExport can only be used on methods")
  }

  @Test def wasmInteropMustStaticOwner(): Unit = {
    val nonStaticOwner =
      """
      class A {
        @WasmImport("env", "imported")
        def imported(): Unit = wasm.native

        @WasmExport("exported")
        def exported(): Unit = ???
      }
      """
    nonStaticOwner.containsErrors(
        "@WasmImport can only be used on methods in static Scala objects")
    nonStaticOwner.containsErrors(
        "@WasmExport can only be used on methods in static Scala objects")

    val objectInClass =
      """
      class Enclosing {
        object A {
          @WasmImport("env", "imported")
          def imported(): Unit = wasm.native

          @WasmExport("exported")
          def exported(): Unit = ???
        }
      }
      """
    objectInClass.containsErrors(
        "@WasmImport can only be used on methods in static Scala objects")
    objectInClass.containsErrors(
        "@WasmExport can only be used on methods in static Scala objects")

    val objectInObject =
      """
      object Enclosing {
        object A {
          @WasmImport("env", "imported")
          def imported(): Unit = wasm.native

          @WasmExport("exported")
          def exported(): Unit = ???
        }
      }
      """
    objectInObject.hasNoWarns()

    val jsOwner =
      """
      object A extends js.Object {
        @WasmImport("env", "imported")
        def imported(): Unit = wasm.native

        @WasmExport("exported")
        def exported(): Unit = ???
      }
      """
    jsOwner.containsErrors(
        "@WasmImport can only be used on methods in static Scala objects")
    jsOwner.containsErrors(
        "@WasmExport can only be used on methods in static Scala objects")

    val local =
      """
      object A {
        def outer(): Unit = {
          @WasmImport("env", "imported")
          def imported(): Unit = wasm.native

          @WasmExport("exported")
          def exported(): Unit = ???
        }
      }
      """
    local.containsErrors("@WasmImport is not allowed on local definitions")
    local.containsErrors("@WasmExport is not allowed on local definitions")
  }

  @Test def wasmInteropNonPublic(): Unit = {
    val privateMethods =
      """
      object A {
        @WasmImport("env", "imported")
        private def imported(): Unit = wasm.native

        @WasmExport("exported")
        private def exported(): Unit = ???
      }
      """
    privateMethods.containsErrors("@WasmImport can only be used on public methods")
    privateMethods.containsErrors("@WasmExport can only be used on public methods")

    val protectedMethods =
      """
      object A {
        @WasmImport("env", "imported")
        protected def imported(): Unit = wasm.native

        @WasmExport("exported")
        protected def exported(): Unit = ???
      }
      """
    protectedMethods.containsErrors("@WasmImport can only be used on public methods")
    protectedMethods.containsErrors("@WasmExport can only be used on public methods")

    val packagePrivateMethods =
      """
      object A {
        @WasmImport("env", "imported")
        private[A] def imported(): Unit = wasm.native

        @WasmExport("exported")
        private[A] def exported(): Unit = ???
      }
      """
    packagePrivateMethods.containsErrors("@WasmImport can only be used on public methods")
    packagePrivateMethods.containsErrors("@WasmExport can only be used on public methods")
  }

  @Test def wasmInteropMethods(): Unit = {
    val typeParams =
      """
      object A {
        @WasmImport("env", "imported")
        def imported[A](x: Int): Unit = wasm.native

        @WasmExport("exported")
        def exported[A](x: Int): Unit = ???
      }
      """

    typeParams.containsErrors("@WasmImport methods may not have type parameters")
    typeParams.containsErrors("@WasmExport methods may not have type parameters")

    val multiParams =
      """
      object A {
        @WasmImport("env", "imported")
        def imported(x: Int)(y: Int): Unit = wasm.native

        @WasmExport("exported")
        def exported(x: Int)(y: Int): Unit = ???
      }
      """

    multiParams.containsErrors("@WasmImport methods may not have multiple parameter lists")
    multiParams.containsErrors("@WasmExport methods may not have multiple parameter lists")

    val noParams =
      """
      object A {
        @WasmImport("env", "imported")
        def imported: Int = wasm.native

        @WasmExport("exported")
        def exported: Int = ???
      }
      """

    noParams.containsErrors("@WasmImport methods must have a parameter list")
    noParams.containsErrors("@WasmExport methods must have a parameter list")
  }

  @Test def wasmInteropParameterTypes(): Unit = {
    val repeated =
      """
      object A {
        @WasmImport("env", "imported")
        def imported(x: Int*): Unit = wasm.native

        @WasmExport("exported")
        def exported(x: Int*): Unit = ???
      }
      """

    repeated.containsErrors("@WasmImport methods may not have repeated parameters")
    repeated.containsErrors("@WasmExport methods may not have repeated parameters")

    val default =
      """
      object A {
        @WasmImport("env", "imported")
        def imported(x: Int = 1): Unit = wasm.native

        @WasmExport("exported")
        def exported(x: Int = 1): Unit = ???
      }
      """

    default.containsErrors("@WasmImport methods may not have default parameters")
    default.containsErrors("@WasmExport methods may not have default parameters")
  }

  @Test def wasmInteropCannotBeJSNative(): Unit = {
    val jsNative =
      """
      object A {
        @js.native
        @WasmImport("env", "imported")
        def imported(): Unit = js.native

        @js.native
        @WasmExport("exported")
        def exported(): Unit = ???
      }
      """

    jsNative.containsErrors("@WasmImport cannot be used together with @js.native")
    jsNative.containsErrors("@WasmExport cannot be used together with @js.native")
  }

  @Test def invalidWasmInteropTypes(): Unit = {
    val message = "Wasm imports and exports only support Int, Long, Float, " +
      "Double, arrays of Byte, Short, Int, Long, Float and Double, " +
      "and Unit as result type"
    def testInvalidParam(tpe: String): Unit = {
      s"""
        object Imports {
          @WasmImport("env", "imported")
          def imported(x: $tpe): Unit = wasm.native
        }
        """.containsErrors(message)

      s"""
        object Exports {
          @WasmExport("exported")
          def exported(x: $tpe): Unit = ???
        }
        """.containsErrors(message)
    }

    def testInvalidResult(tpe: String): Unit = {
      s"""
        object Exports {
          @WasmExport("exported")
          def exported(): $tpe = ???
        }
        """.containsErrors(message)
    }

    for (t <- List(
            "Byte",
            "Short",
            "Boolean",
            "String",
            "Char",
            "Array[Boolean]",
            "Array[Char]",
            "Array[String]",
            "Array[Array[Int]]",
            "Any",
            "AnyRef",
            "Nothing",
            "Null",
            "java.lang.Integer",
            "Array[java.lang.Integer]",
            "List[Int]"
        )) {
      testInvalidParam(t)
      testInvalidResult(t)
    }

    if (!scala.util.Properties.versionNumberString.startsWith("2.12.")) {
      testInvalidParam("1")
      testInvalidResult("1")
    }

    testInvalidParam("Unit")
  }

  @Test def invalidWasmImportBody(): Unit = {
    val errors = """
    object A {
      @WasmImport("env", "f")
      def imported(x: Int): Int = x

      @WasmImport("env", "g")
      def importedUnit(): Unit = ()
    }
    """

    errors.containsErrors("@WasmImport methods may only call wasm.native.")
  }

  @Test def duplicateWasmExportNames(): Unit = {
    """
    object A {
      @WasmExport("f")
      def exported1(): Unit = ???

      @WasmExport("f")
      def exported2(): Unit = ???
    }
    """.containsErrors("Duplicate @WasmExport name 'f' already used at")
  }
}
