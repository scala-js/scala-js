# Scala.js, a Scala to JavaScript compiler

Scala.js compiles Scala code to JavaScript, allowing you to write your
Web application entirely in Scala!

Outstanding features are:

*   Support all of Scala (including macros!),
    modulo [a few semantic differences](http://www.scala-js.org/doc/semantics.html)
*   Very good [interoperability with JavaScript code](http://www.scala-js.org/doc/js-interoperability.html).
    For example, use jQuery and HTML5 from your Scala.js code, either in a
    typed or untyped way. Or create Scala.js objects and call their methods
    from JavaScript.
*   Integrated with [sbt](http://www.scala-sbt.org/)
    (including support for dependency management and incremental compilation)
*   Can be used with your favorite IDE for Scala
*   Generates [Source Maps](http://www.html5rocks.com/en/tutorials/developertools/sourcemaps/)
    for a smooth debugging experience (step through your Scala code from within
    your browser supporting source maps)
*   Integrates [Google Closure Compiler](https://developers.google.com/closure/compiler/)
    for producing minimal code for production.

## Resources

*   [Website](http://www.scala-js.org/)
*   [Mailing list](https://groups.google.com/forum/?fromgroups#!forum/scala-js)

## Get started

We provide a
[bootstrapping application](https://github.com/sjrd/scala-js-example-app)
which you can fork to kick off your own project. Its readme provides further
explanations on how to do so.

## Contribute

### Get the code

Get the code with git. Beware that this repo contains one submodule, which
you need to clone too. To do this, add the `--recursive` option when cloning.
If you've already cloned this repo, cd into it and do:

    $ git submodule init
    $ git submodule update

### Compile

Scala.js uses [sbt](http://www.scala-sbt.org/) for its build process.
To compile your fork, simply run:

    sbt> package

### Run the test suite

Compile and run the test suite with

    sbt> scalajs-test/test

(you must have run `package` before running the test suite)

### Test the examples

After having compiled Scala.js, you can compile the example applications with:

    sbt> examples/optimizeJS

(you must have run `package` before compiling the examples)

Then, you can "execute" them by opening their respective HTML files in your
favorite browser. Since optimizing the JavaScript takes time (tens of seconds
to several minutes, depending on your hardware), it is also possible not to
optimize JS by doing instead:

    sbt> examples/packageJS

In this case, you have to open the `-dev` version of the HTML files.

Currently, two examples are provided:

*   `examples/helloworld/helloworld.html`, saying Hello World in four different
    ways (using DOM or jQuery, and using the untyped or typed interface to
    JavaScript).
*   `examples/reversi/reversi.html`, an implementation of a
    [Reversi](http://en.wikipedia.org/wiki/Reversi) game. Note that it uses the
    HTML5 Canvas element, so it won't work with Internet Explorer 8 or below.

### Use your fork with your own projects

Simply publish it locally with:

    sbt> publishLocal

## License

Scala.js is distributed under the
[Scala License](http://www.scala-lang.org/node/146).
