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

import java.nio.*;

/**
 * Utilities to interface {@link java.nio.Buffer}s and JavaScript TypedArrays.
 *
 * <p>{@link java.nio.Buffer}s can be <em>direct</em> buffers or
 * <em>indirect</em> buffers. Indirect buffers use an underlying array (like
 * {@code int[]} in Java or {@code Array[Int]} in Scala). Direct buffers are
 * supposed to use off-heap memory.
 *
 * <p>In a JavaScript environment, the equivalent of off-heap memory for
 * buffers of primitive numeric types are TypedArrays.
 *
 * <p>This class provides methods to wrap TypedArrays as direct Buffers, and
 * extract references to TypedArrays from direct Buffers.
 */
public final class TypedArrayBuffer {
  private TypedArrayBuffer() {}

  /**
   * Wraps a JavaScript {@code Int8Array} as a direct
   * {@link java.nio.ByteBuffer}.
   *
   * <p>The provided {@code array} parameter must be a valid JavaScript
   * {@code Int8Array}, otherwise the behavior of this method is not
   * specified.
   *
   * <p>The returned {@link java.nio.ByteBuffer} has the following properties:
   *
   * <ul>
   *   <li>It has a {@code capacity()} equal to the {@code array.length}.</li>
   *   <li>Its initial {@code position()} is 0 and its {@code limit()} is its capacity.</li>
   *   <li>It is a direct buffer backed by the provided {@code Int8Array}:
   *     changes to one are reflected on the other.</li>
   * </ul>
   *
   * @param array a JavaScript {@code Int8Array}
   */
  public static final ByteBuffer wrapInt8Array(Object array) {
    throw new AssertionError("stub");
  }

  /**
   * Wraps a JavaScript {@code Uint16Array} as a direct
   * {@link java.nio.CharBuffer}.
   *
   * <p>The provided {@code array} parameter must be a valid JavaScript
   * {@code Uint16Array}, otherwise the behavior of this method is not
   * specified.
   *
   * <p>The returned {@link java.nio.CharBuffer} has the following properties:
   *
   * <ul>
   *   <li>It has a {@code capacity()} equal to the {@code array.length}.</li>
   *   <li>Its initial {@code position()} is 0 and its {@code limit()} is its capacity.</li>
   *   <li>It is a direct buffer backed by the provided {@code Uint16Array}:
   *     changes to one are reflected on the other.</li>
   * </ul>
   *
   * @param array a JavaScript {@code Uint16Array}
   */
  public static final CharBuffer wrapUint16Array(Object array) {
    throw new AssertionError("stub");
  }

  /**
   * Wraps a JavaScript {@code Int16Array} as a direct
   * {@link java.nio.ShortBuffer}.
   *
   * <p>The provided {@code array} parameter must be a valid JavaScript
   * {@code Int16Array}, otherwise the behavior of this method is not
   * specified.
   *
   * <p>The returned {@link java.nio.ShortBuffer} has the following properties:
   *
   * <ul>
   *   <li>It has a {@code capacity()} equal to the {@code array.length}.</li>
   *   <li>Its initial {@code position()} is 0 and its {@code limit()} is its capacity.</li>
   *   <li>It is a direct buffer backed by the provided {@code Int16Array}:
   *     changes to one are reflected on the other.</li>
   * </ul>
   *
   * @param array a JavaScript {@code Int16Array}
   */
  public static final ShortBuffer wrapInt16Array(Object array) {
    throw new AssertionError("stub");
  }

  /**
   * Wraps a JavaScript {@code Int32Array} as a direct
   * {@link java.nio.IntBuffer}.
   *
   * <p>The provided {@code array} parameter must be a valid JavaScript
   * {@code Int32Array}, otherwise the behavior of this method is not
   * specified.
   *
   * <p>The returned {@link java.nio.IntBuffer} has the following properties:
   *
   * <ul>
   *   <li>It has a {@code capacity()} equal to the {@code array.length}.</li>
   *   <li>Its initial {@code position()} is 0 and its {@code limit()} is its capacity.</li>
   *   <li>It is a direct buffer backed by the provided {@code Int32Array}:
   *     changes to one are reflected on the other.</li>
   * </ul>
   *
   * @param array a JavaScript {@code Int32Array}
   */
  public static final IntBuffer wrapInt32Array(Object array) {
    throw new AssertionError("stub");
  }

  /**
   * Wraps a JavaScript {@code Float32Array} as a direct
   * {@link java.nio.FloatBuffer}.
   *
   * <p>The provided {@code array} parameter must be a valid JavaScript
   * {@code Float32Array}, otherwise the behavior of this method is not
   * specified.
   *
   * <p>The returned {@link java.nio.FloatBuffer} has the following properties:
   *
   * <ul>
   *   <li>It has a {@code capacity()} equal to the {@code array.length}.</li>
   *   <li>Its initial {@code position()} is 0 and its {@code limit()} is its capacity.</li>
   *   <li>It is a direct buffer backed by the provided {@code Float32Array}:
   *     changes to one are reflected on the other.</li>
   * </ul>
   *
   * @param array a JavaScript {@code Float32Array}
   */
  public static final FloatBuffer wrapFloat32Array(Object array) {
    throw new AssertionError("stub");
  }

  /**
   * Wraps a JavaScript {@code Float64Array} as a direct
   * {@link java.nio.DoubleBuffer}.
   *
   * <p>The provided {@code array} parameter must be a valid JavaScript
   * {@code Float64Array}, otherwise the behavior of this method is not
   * specified.
   *
   * <p>The returned {@link java.nio.DoubleBuffer} has the following properties:
   *
   * <ul>
   *   <li>It has a {@code capacity()} equal to the {@code array.length}.</li>
   *   <li>Its initial {@code position()} is 0 and its {@code limit()} is its capacity.</li>
   *   <li>It is a direct buffer backed by the provided {@code Float64Array}:
   *     changes to one are reflected on the other.</li>
   * </ul>
   *
   * @param array a JavaScript {@code Float64Array}
   */
  public static final DoubleBuffer wrapFloat64Array(Object array) {
    throw new AssertionError("stub");
  }

  /**
   * Tests whether the given {@link java.nio.Buffer} is backed by an accessible
   * JavaScript {@code ArrayBuffer}.
   *
   * <p>This is true for all read-write direct buffers, in particular for those
   * created with any of the {@code wrapX} methods of this class.
   *
   * <p>If this method returns {@code true}, then {@code arrayBuffer(buffer)},
   * {@code arrayBufferOffset(buffer)} and {@code dataView(buffer)} do not
   * throw any {@link UnsupportedOperationException}.
   *
   * @return
   *   true if and only if the provided {@code buffer} is backed by an
   *   accessible JavaScript {@code ArrayBuffer}
   *
   * @see TypedArrayBuffer#arrayBuffer(Buffer)
   * @see TypedArrayBuffer#arrayBufferOffset(Buffer)
   * @see TypedArrayBuffer#dataView(Buffer)
   */
  public static final boolean hasArrayBuffer(Buffer buffer) {
    throw new AssertionError("stub");
  }

  /**
   * Returns the JavaScript {@code ArrayBuffer} backing the provided
   * {@link java.nio.Buffer}.
   *
   * <p>The {@code buffer} may represent a <em>view</em> of the returned
   * {@code ArrayBuffer} that does not start at index 0. Use the method
   * {@link TypedArrayBuffer#arrayBufferOffset(Buffer)} to retrieve the offset
   * within the {@code ArrayBuffer}.
   *
   * @return
   *   the JavaScript {@code ArrayBuffer} backing the provided {@code buffer}
   *
   * @throws UnsupportedOperationException
   *   if the provided {@code buffer} is read-only or is not backed by a
   *   JavaScript {@code ArrayBuffer}, i.e., if {@code hasArrayBuffer(buffer)}
   *   returns {@code false}
   *
   * @see TypedArrayBuffer#hasArrayBuffer(Buffer)
   * @see TypedArrayBuffer#arrayBufferOffset(Buffer)
   */
  public static final Object arrayBuffer(Buffer buffer) throws UnsupportedOperationException {
    throw new AssertionError("stub");
  }

  /**
   * Returns the offset within the JavaScript {@code ArrayBuffer} backing the
   * provided {@link java.nio.Buffer}.
   *
   * @return
   *   the offset within the JavaScript {@code ArrayBuffer} backing the
   *   provided {@code buffer} where the latter starts
   *
   * @throws UnsupportedOperationException
   *   if the provided {@code buffer} is read-only or is not backed by a
   *   JavaScript {@code ArrayBuffer}, i.e., if {@code hasArrayBuffer(buffer)}
   *   returns {@code false}
   *
   * @see TypedArrayBuffer#hasArrayBuffer(Buffer)
   * @see TypedArrayBuffer#arrayBuffer(Buffer)
   */
  public static final int arrayBufferOffset(Buffer buffer) throws UnsupportedOperationException {
    throw new AssertionError("stub");
  }

  /**
   * Returns a JavaScript {@code DataView} of the provided
   * {@link java.nio.Buffer}.
   *
   * @return
   *   a JavaScript {@code DataView} of the provided {@code buffer}
   *
   * @throws UnsupportedOperationException
   *   if the provided {@code buffer} is read-only or is not backed by a
   *   JavaScript {@code ArrayBuffer}, i.e., if {@code hasArrayBuffer(buffer)}
   *   returns {@code false}
   *
   * @see TypedArrayBuffer#hasArrayBuffer(Buffer)
   */
  public static final Object dataView(Buffer buffer) throws UnsupportedOperationException {
    throw new AssertionError("stub");
  }

  /**
   * Tests whether the given {@link java.nio.Buffer} is backed by an accessible
   * JavaScript {@code TypedArray}.
   *
   * <p>This is true when all of the following conditions apply:
   *
   * <ul>
   *   <li>the buffer is a <em>direct</em> buffer,</li>
   *   <li>it is not read-only,</li>
   *   <li>its byte order corresponds to the native byte order of JavaScript
   *     {@code TypedArray}s, and</li>
   *   <li>it is not a {@link java.nio.LongBuffer}.</li>
   * </ul>
   *
   * <p>In particular, it is true for all {@link java.nio.Buffer}s created with
   * any of the {@code wrapXArray} methods of this class.
   *
   * <p>If this method returns {@code true}, then {@code typedArray(buffer)}
   * does not throw any {@link UnsupportedOperationException}.
   *
   * @return
   *   true if and only if the provided {@code buffer} is backed by an
   *   accessible JavaScript {@code TypedArray}
   *
   * @see TypedArrayBuffer#typedArray(Buffer)
   */
  public static final boolean hasTypedArray(Buffer buffer) {
    throw new AssertionError("stub");
  }

  /**
   * Returns a JavaScript {@code TypedArray} view of the provided
   * {@link java.nio.Buffer}.
   *
   * <p>The particular type of {@code TypedArray} depends on the type of buffer:
   *
   * <ul>
   *   <li>an {@code Int8Array} for a {@link java.nio.ByteBuffer}</li>
   *   <li>a {@code Uint16Array} for a {@link java.nio.CharBuffer}</li>
   *   <li>an {@code Int16Array} for a {@link java.nio.ShortBuffer}</li>
   *   <li>an {@code Int32Array} for a {@link java.nio.IntBuffer}</li>
   *   <li>an {@code Float32Array} for a {@link java.nio.FloatBuffer}</li>
   *   <li>an {@code Float64Array} for a {@link java.nio.DoubleBuffer}</li>
   * </ul>
   *
   * @return
   *   a JavaScript {@code TypedArray} view of the provided {@code buffer}
   *
   * @throws UnsupportedOperationException
   *   if the provided {@code buffer} is read-only or is not backed by a
   *   JavaScript {@code TypedArray}, i.e., if {@code hasTypedArray(buffer)}
   *   returns {@code false}
   *
   * @see TypedArrayBuffer#hasTypedArray(Buffer)
   */
  public static final Object typedArray(Buffer buffer) throws UnsupportedOperationException {
    throw new AssertionError("stub");
  }
}
