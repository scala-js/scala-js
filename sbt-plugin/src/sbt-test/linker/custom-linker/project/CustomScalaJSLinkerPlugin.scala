package build

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.collection.JavaConverters._

import sbt._
import sbt.Keys._

import org.scalajs.linker._
import org.scalajs.linker.interface.{ModuleKind, _}

import org.scalajs.sbtplugin._
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

object CustomScalaJSLinkerPlugin extends AutoPlugin {
  override lazy val requires = ScalaJSPlugin

  object autoImport {
    val scalaJSImportedModules = taskKey[List[String]]("imported modules")
  }

  import autoImport._

  private class CustomLinkerImpl(base: LinkerImpl.Reflect)
      extends LinkerImpl.Forwarding(base) {

    private val loader = base.loader

    private val clearableLinkerMethod = {
      Class.forName("customlinker.CustomLinkerImpl", true, loader)
        .getMethod("clearableLinker", classOf[StandardConfig], classOf[Path])
    }

    def customLinker(config: StandardConfig,
        entryPointOutputFile: Path): ClearableLinker = {
      clearableLinkerMethod.invoke(null, config, entryPointOutputFile)
        .asInstanceOf[ClearableLinker]
    }
  }

  private def scalaJSStageSettings(stage: Stage,
      key: TaskKey[Attributed[Report]]): Seq[Setting[_]] = {
    val entryPointOutputFileName =
      s"entrypoints-${stage.toString.toLowerCase}.txt"

    Def.settings(
      key / scalaJSLinker := {
        val config = (key / scalaJSLinkerConfig).value
        val box = (key / scalaJSLinkerBox).value
        val linkerImpl = (key / scalaJSLinkerImpl).value
        val projectID = thisProject.value.id
        val configName = configuration.value.name
        val log = streams.value.log
        val entryPointOutputFile = crossTarget.value / entryPointOutputFileName

        if (config.moduleKind != scalaJSLinkerConfig.value.moduleKind) {
          val keyName = key.key.label
          log.warn(
              s"The module kind in `scalaJSLinkerConfig in ($projectID, " +
              s"$configName, $keyName)` is different than the one `in " +
              s"`($projectID, $configName)`. " +
              "Some things will go wrong.")
        }

        box.ensure {
          linkerImpl.asInstanceOf[CustomLinkerImpl].customLinker(config,
              entryPointOutputFile.toPath)
        }
      },

      key / scalaJSImportedModules := {
        val _ = key.value
        val linker = (key / scalaJSLinker).value
        val entryPointOutputFile = crossTarget.value / entryPointOutputFileName
        val lines = Files.readAllLines(entryPointOutputFile.toPath,
            StandardCharsets.UTF_8)
        lines.asScala.toList
      }
    )
  }

  override def globalSettings: Seq[Setting[_]] = Def.settings(
    scalaJSLinkerImpl := {
      val cp = (scalaJSLinkerImpl / fullClasspath).value
      scalaJSLinkerImplBox.value.ensure {
        new CustomLinkerImpl(LinkerImpl.reflect(Attributed.data(cp)))
      }
    }
  )

  private lazy val configSettings: Seq[Setting[_]] = Def.settings(
    scalaJSStageSettings(FastOptStage, fastLinkJS),
    scalaJSStageSettings(FullOptStage, fullLinkJS),
  )

  override def projectSettings: Seq[Setting[_]] = Def.settings(
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },

    inConfig(Compile)(configSettings),
    inConfig(Test)(configSettings),
  )
}
