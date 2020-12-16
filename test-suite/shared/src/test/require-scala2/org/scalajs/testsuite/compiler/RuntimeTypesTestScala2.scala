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

package org.scalajs.testsuite.compiler

import org.junit.Test

class RuntimeTypesTestScala2 {

  /* The following tests are Scala 2.x only because `Array[Null]` and
   * `Array[Nothing]` are not officially supported in Scala 3. As of this
   * writing, they *can* be made to work by actively avoiding the `ClassTag`s,
   * but they might break at any point in the future.
   */

  @Test def scalaNothingArrayNothingCanExistAndIsCastable(): Unit = {
    val arr = Array[Nothing]()
    arr.asInstanceOf[Array[Nothing]]
  }

  @Test def scalaNothingArrayArrayNothingToo(): Unit = {
    val arr = Array[Array[Nothing]]()
    arr.asInstanceOf[Array[Array[Nothing]]]
    // This apparently works too... Dunno why
    arr.asInstanceOf[Array[Nothing]]
  }

  @Test def scalaNullArrayNullCanExistAndIsCastable(): Unit = {
    val arr = Array.fill[Null](5)(null)
    arr.asInstanceOf[Array[Null]]
  }

  @Test def scalaNullArrayArrayNullToo(): Unit = {
    // Was `val arr = Array.fill[Null](5, 5)(null)` but that crashes on the JVM
    val arr = new Array[Array[Null]](5)
    arr.asInstanceOf[Array[Array[Null]]]
    // This apparently works too... Dunno why
    arr.asInstanceOf[Array[Null]]
  }

}
