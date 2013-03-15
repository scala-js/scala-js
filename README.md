# Scala-JS, a JavaScript backend for Scala

This project aims at providing a JavaScript backend for Scala.

## Get it

Get the code with git. Beware that this repo contains two submodules, which
you need to clone too. So after cloning this repo, cd into it and do:

    $ git submodule init
    $ git submodule update

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
