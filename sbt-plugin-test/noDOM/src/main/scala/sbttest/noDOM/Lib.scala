package sbttest.noDOM

object Lib {

  /** appends `_foo` to a string */
  def foo(x: String): String = x + "foo"

  /** squares a number */
  def sq(x: Int): Int = x * x

}
