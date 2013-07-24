/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.js

class Error extends Object {
  def this(message: String) = this()

  val name: String = ???
  val message: String = ???
}

object Error extends Object {
  def apply(message: String): Error = ???
  def apply(): Error = ???
}

class EvalError extends Error {
  def this(message: String) = this()
}

object EvalError extends Object {
  def apply(message: String): EvalError = ???
  def apply(): EvalError = ???
}

class RangeError extends Error {
  def this(message: String) = this()
}

object RangeError extends Object {
  def apply(message: String): RangeError = ???
  def apply(): RangeError = ???
}

class ReferenceError extends Error {
  def this(message: String) = this()
}

object ReferenceError extends Object {
  def apply(message: String): ReferenceError = ???
  def apply(): ReferenceError = ???
}

class SyntaxError extends Error {
  def this(message: String) = this()
}

object SyntaxError extends Object {
  def apply(message: String): SyntaxError = ???
  def apply(): SyntaxError = ???
}

class TypeError extends Error {
  def this(message: String) = this()
}

object TypeError extends Object {
  def apply(message: String): TypeError = ???
  def apply(): TypeError = ???
}

class URIError extends Error {
  def this(message: String) = this()
}

object URIError extends Object {
  def apply(message: String): URIError = ???
  def apply(): URIError = ???
}
