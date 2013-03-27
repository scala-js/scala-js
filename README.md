# Scala.js, a JavaScript backend for Scala

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
compile the example applications:

    sbt> examples/package-js

You can "execute" the example applications by opening their respective HTML
files in your favorite browser.

Currently, two examples are provided:

*   `examples/helloworld/helloworld.html`, saying Hello World in four different
    ways (using DOM or jQuery, and using the untyped or typed interface to
    JavaScript).
*   `examples/reversi/reversi.html`, an implementation of a
    [Reversi](http://en.wikipedia.org/wiki/Reversi) game. Note that it uses the
    HTML5 Canvas element, so it won't work with Internet Explorer 8 or below.

## License

Scala.js is distributed under the
[Scala License](http://www.scala-lang.org/node/146).
