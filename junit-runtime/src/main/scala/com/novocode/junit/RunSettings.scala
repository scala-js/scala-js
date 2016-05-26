package com.novocode.junit

import com.novocode.junit.Ansi._

import java.util.HashSet

import scala.util.Try

class RunSettings private (val color: Boolean, val decodeScalaNames: Boolean,
    val quiet: Boolean, val verbose: Boolean, val logAssert: Boolean,
    val notLogExceptionClass: Boolean) {

  private val ignoreRunnersSet = new HashSet[String]

  def this(color: Boolean, decodeScalaNames: Boolean, quiet: Boolean,
      verbose: Boolean, logAssert: Boolean, ignoreRunners: String,
      notLogExceptionClass: Boolean) = {
    this(color, decodeScalaNames, quiet, verbose, logAssert, notLogExceptionClass)
    for (s <- ignoreRunners.split(","))
      ignoreRunnersSet.add(s.trim)
  }

  def decodeName(name: String): String =
    if (decodeScalaNames) RunSettings.decodeScalaName(name) else name

  def buildColoredMessage(t: Throwable, c1: String): String = {
    if (t == null) "null" else {
      if (notLogExceptionClass || (!logAssert && t.isInstanceOf[AssertionError])) {
        t.getMessage
      } else {
        val b = new StringBuilder()
        val cn = decodeName(t.getClass.getName)
        val pos1 = cn.indexOf('$')
        val pos2 = {
          if (pos1 == -1) cn.lastIndexOf('.')
          else cn.lastIndexOf('.', pos1)
        }
        if (pos2 == -1) b.append(c(cn, c1))
        else {
          b.append(cn.substring(0, pos2))
          b.append('.')
          b.append(c(cn.substring(pos2 + 1), c1))
        }
        b.append(": ").append(t.getMessage)
        b.toString()
      }
    }
  }

  def buildInfoMessage(t: Throwable): String =
    buildColoredMessage(t, NNAME2)

  def buildErrorMessage(t: Throwable): String =
    buildColoredMessage(t, ENAME2)
}

object RunSettings {
  private[RunSettings] def decodeScalaName(name: String): String =
    Try(scala.reflect.NameTransformer.decode(name)).getOrElse(name)
}
