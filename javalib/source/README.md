# Java standard library written in Scala

This project aims at implementing a (partial) Java standard library in Scala.

Scala is a powerful language, with a very modular compiler, which allows to
develop several backends (or targets). The issue with non-JVM backends is that
they cannot use the standard Java library, which is written in Java. Now, the
Scala library and core class hierarchy relies on the Java standard library,
which forces the developers of non-JVM backends to hack some parts of it.

This project aims at solving this issue, by developing a common implementation
of the Java standard library in Scala, so that it can be compiled by the Scala
compiler with any backend.

Examples of backends for Scala that can benefit from this library:

* [JavaScript backend](https://github.com/lampepfl/scala-js)
* [LLVM backend](https://github.com/greedy/scala-llvm)
* [Mozart backend of Ozma](https://github.com/sjrd/ozma)
