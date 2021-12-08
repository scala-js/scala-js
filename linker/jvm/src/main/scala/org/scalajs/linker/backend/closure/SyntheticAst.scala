/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.linker.backend.closure

import com.google.javascript.jscomp.{AbstractCompiler, SourceAst, SourceFile}
import com.google.javascript.rhino.{IR, InputId, Node}
import com.google.javascript.rhino.StaticSourceFile.SourceKind

/** An AST generated totally by the compiler.
 *
 *  This is a port of what we need from
 *  `com.google.javascript.jscomp.SyntheticAst`, before it was removed from the
 *  upstream repo in
 *  https://github.com/google/closure-compiler/commit/7a53987dd77dcc69511a42f58606f9d77709a50e
 */
private[closure] final class SyntheticAst(private var root: Node) extends SourceAst {
  private val inputId = new InputId(root.getSourceFileName())
  private val sourceFile = SourceFile.fromCode(root.getSourceFileName(), "", SourceKind.STRONG)

  def getAstRoot(compiler: AbstractCompiler): Node = root

  def clearAst(): Unit = {
    root = IR.script()
    root.setInputId(inputId)
    root.setStaticSourceFile(sourceFile)
  }

  def getInputId(): InputId = inputId

  def getSourceFile(): SourceFile = sourceFile
}
