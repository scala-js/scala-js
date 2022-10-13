package app

object Main {
  def main(args: Array[String]): Unit = {
    println(testlib.ADT.SingletonCase) // #4739
    println(testlib.ADT.ClassCase("foo"))
  }
}
