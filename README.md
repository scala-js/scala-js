# Scala-JS, a JavaScript backend for Scala

This project aims at providing a JavaScript backend for Scala.

## Test it

Build is done with [sbt](http://www.scala-sbt.org/). Use

    sbt> package-js

to compile the compiler and the standard library. Afterwards, you can
compile the example application Hello World:

    sbt> helloworld/package-js

You can "execute" that application by opening the file
`examples/helloworld/helloworld.html` in your favorite browser.

## License

Scala-JS is distributed under the
[Scala License](http://www.scala-lang.org/node/146).
