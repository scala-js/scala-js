package sbttest.multitest.test

import sbttest.multitest.Lib
import sbttest.framework.Test

class LibTest extends Test {
  assert(Lib.sq(2) == 4)
  assert(Lib.sq(4) == 16)
}
