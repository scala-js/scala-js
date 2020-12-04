This file contains test cases that should be manually executed.

## HTML-Runners

The following HTML-runners must be manually tested:

    examples/helloworld/helloworld-{2.11|2.12}{|-fastopt}.html
    examples/reversi/reversi-{2.11|2.12}{|-fastopt}.html

## Sourcemaps

To test source maps, do the following on:

    examples/reversi/reversi-{2.11|2.12}{|-fastopt}.html

1. Open the respective file in Google Chrome
2. Set a break-point in the HTML launcher on the `new Reversi` statement
3. Step over calls to jQuery into constructor
4. Step into the call to `Array.tabulate` and verify that source maps
   to Scala standard library sources work (should point to GitHub)
5. Single step through constructor, until you reach `buildUI()`
6. Step into `buildUI()`


## When releasing only

Once all tests pass, tag the revision and verify that source maps to
Scala.js sources work correctly (should point to GitHub), following
the steps described in the section Sourcemaps.
