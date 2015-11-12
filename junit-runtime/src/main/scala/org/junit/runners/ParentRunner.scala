package org.junit.runners

import org.junit.runner.Runner

// Dummy for classOf[...]
abstract class ParentRunner[T](testClass: Class[_]) extends Runner
