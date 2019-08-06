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

package org.scalajs.jsenv

/** A JavaScript execution environment.
 *
 *  This can run and interact with JavaScript code.
 *
 *  Any implementation is expected to be fully thread-safe.
 */
trait JSEnv {
  /** Human-readable name for this [[JSEnv]] */
  val name: String

  /** Starts a new (asynchronous) JS run.
   *
   *  This may only throw if value of `input` is unknown or `config` cannot be
   *  supported. To verify whether a [[RunConfig]] can be supported in a forward
   *  compatible manner (i.e. when new options are added in later versions)
   *  implementations of [[JSEnv]]s must use [[RunConfig.Validator]].
   *
   *  This must not throw if the run cannot be started or there is a problem
   *  with the input's content (e.g. file does not exist, syntax error, etc.).
   *  In this case, [[JSRun#future]] should be failed instead.
   *
   *  @throws UnsupportedInputException if the value of `input` cannot be
   *      supported.
   *  @throws java.lang.IllegalArgumentException if the value of `config` cannot
   *      be supported.
   */
  def start(input: Seq[Input], config: RunConfig): JSRun

  /** Like [[start]], but initializes a communication channel.
   *
   *  Inside the VM this is to provide a global JavaScript object named
   *  `scalajsCom` that can be used to interact with the message channel. Its
   *  operations are:
   *  {{{
   *  // initialize com (with callback). May only be called once.
   *  scalajsCom.init(function(msg) { console.log("Received: " + msg); });
   *
   *  // send a message to host system
   *  scalajsCom.send("my message");
   *  }}}
   *
   *  All messages, sent in both directions, must be valid UTF-16 strings,
   *  i.e., they must not contain any unpaired surrogate character. The
   *  behavior of a communication channel is unspecified if this requirement is
   *  not met.
   *
   *  We describe the expected message delivery guarantees by denoting the
   *  transmitter as `t` and  the receiver as `r`. Both the JVM and the JS end
   *  act once as a transmitter and once as a receiver. These two
   *  transmitter/receiver pairs (JS/JVM and JVM/JS) are independent.
   *
   *  For a pair `(t,r)`:
   *  <ul>
   *  <li>If `t` calls [[JSComRun#send]] exactly in the sequence
   *  {{{
   *  send(m_1), ..., send(m_n)
   *  }}}
   *
   *  and `r` observes `onMessage(m_k)` (k <= n) but not `onMessage(m_{k+1})`,
   *  `r` must observe
   *  {{{
   *  onMessage(m_1), ..., onMessage(m_k)
   *  }}}
   *  exactly in this order.
   *  <li>If `t` and `r` keep running indefinitely and `t` sends n messages,
   *  `r` receives n messages.
   *  </ul>
   *
   *  @param onMessage Callback invoked each time a message is received from the
   *      JS VM. The implementation may not call this anymore once
   *      [[JSRun#future]] of the returned [[JSComRun]] is completed. Further,
   *      [[JSRun#future]] may only complete with no callback in-flight.
   */
  def startWithCom(input: Seq[Input], config: RunConfig,
      onMessage: String => Unit): JSComRun
}
