package org.scalajs.core.ir

class InvalidIRException(val tree: Trees.Tree, message: String)
    extends Exception(message)
