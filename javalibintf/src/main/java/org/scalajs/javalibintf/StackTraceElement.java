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

package org.scalajs.javalibintf;

/**
 * Scala.js-specific extensions for {@link java.lang.StackTraceElement}.
 *
 * <p>In the JavaScript ecosystem, it is common practice for stack traces to
 * mention column numbers in addition to line numbers. The official API of
 * {@link java.lang.StackTraceElement} does not allow for representing column
 * numbers, but Scala.js supports them.
 *
 * <p>This class offers methods to manipulate the extended information of
 * {@link java.lang.StackTraceElement} for Scala.js.
 *
 * <p>This class only contains static methods. It cannot be instantiated.
 *
 * @see java.lang.StackTraceElement
 */
public final class StackTraceElement {
  private StackTraceElement() {}

  /**
   * Creates a {@link java.lang.StackTraceElement} that includes a column number.
   *
   * @param declaringClass
   *   the fully qualified name of the class containing the execution point
   *   represented by the stack trace element
   * @param methodName
   *   the name of the method containing the execution point represented by the
   *   stack trace element
   * @param fileName
   *   the name of the file containing the execution point represented by the
   *   stack trace element, or null if this information is unavailable
   * @param lineNumber
   *   the line number of the source line containing the execution point
   *   represented by this stack trace element, or a negative number if this
   *   information is unavailable
   * @param columnNumber
   *   the column number within the source line containing the execution point
   *   represented by this stack trace element, or a negative number if this
   *   information is unavailable
   *
   * @return
   *   a new {@link java.lang.StackTraceElement} containing the provided information
   */
  public static final java.lang.StackTraceElement createWithColumnNumber(
      String declaringClass, String methodName, String fileName,
      int lineNumber, int columnNumber) {
    throw new AssertionError("stub");
  }

  /**
   * Returns the column number of the provided {@link java.lang.StackTraceElement}.
   *
   * @return
   *   the column number of the provided stackTraceElement, or a negative
   *   number if this information is unavailable
   */
  public static final int getColumnNumber(
      java.lang.StackTraceElement stackTraceElement) {
    throw new AssertionError("stub");
  }
}
