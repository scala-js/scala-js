package org.scalajs.linker.standard

import java.io.IOException

import scala.concurrent.{ExecutionContext, Future}

import org.scalajs.ir.EntryPointsInfo
import org.scalajs.ir.IRVersionNotSupportedException
import org.scalajs.ir.Trees.ClassDef

import org.scalajs.linker.interface.unstable.IRFileImpl

object ConcreteIRFileImpl {
  def fromIRFileImplEntryPointsInfo(
      entryPointsInfo: IRFileImpl.EntryPointsInfo): EntryPointsInfo =
    entryPointsInfo.asInstanceOf[EntryPointsInfo]

  def fromIRFileImplClassDef(classDef: IRFileImpl.ClassDef): ClassDef =
    classDef.asInstanceOf[ClassDef]

  def toIRFileImplEntryPointsInfo(
      entryPointsInfo: EntryPointsInfo): IRFileImpl.EntryPointsInfo =
    entryPointsInfo.asInstanceOf[IRFileImpl.EntryPointsInfo]

  def toIRFileImplClassDef(classDef: ClassDef): IRFileImpl.ClassDef =
    classDef.asInstanceOf[IRFileImpl.ClassDef]

  def withPathExceptionContext[A](path: String, future: Future[A])(
      implicit ec: ExecutionContext): Future[A] = {
    future.recover {
      case e: IRVersionNotSupportedException =>
        throw new IRVersionNotSupportedException(e.version, e.supported,
            s"Failed to deserialize a file compiled with Scala.js ${e.version}" +
            s" (supported up to: ${e.supported}): $path", e)

      case e: Exception =>
        throw new IOException(s"Failed to deserialize $path", e)
    }
  }
}
