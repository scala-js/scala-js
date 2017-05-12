package org.scalajs.jsdependencies.sbtplugin

import scala.collection.mutable
import scala.util.Try

import java.io.InputStreamReader

import sbt._
import sbt.Keys._

import org.scalajs.core.ir.Utils.escapeJS

import org.scalajs.core.tools.io.{IO => toolsIO, _}
import org.scalajs.core.tools.json._

import org.scalajs.jsenv.VirtualFileMaterializer

import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import org.scalajs.sbtplugin.ScalaJSPluginInternal.scalaJSSourceFiles

import org.scalajs.jsdependencies.core._
import org.scalajs.jsdependencies.core.DependencyResolver.DependencyFilter
import org.scalajs.jsdependencies.core.ManifestFilters.ManifestFilter

object JSDependenciesPlugin extends AutoPlugin {
  override def requires: Plugins = ScalaJSPlugin

  object autoImport {
    import KeyRanks._

    val scalaJSNativeLibraries = TaskKey[Attributed[Seq[VirtualJSFile with RelativeVirtualFile]]](
        "scalaJSNativeLibraries", "All the *.js files on the classpath", CTask)

    val packageJSDependencies = TaskKey[File]("packageJSDependencies",
        "Packages all dependencies of the preLink classpath in a single file.", AMinusTask)

    val packageMinifiedJSDependencies = TaskKey[File]("packageMinifiedJSDependencies",
        "Packages minified version (if available) of dependencies of the preLink " +
        "classpath in a single file.", AMinusTask)

    val jsDependencyManifest = TaskKey[File]("jsDependencyManifest",
        "Writes the JS_DEPENDENCIES file.", DTask)

    val jsDependencyManifests = TaskKey[Attributed[Traversable[JSDependencyManifest]]](
        "jsDependencyManifests", "All the JS_DEPENDENCIES on the classpath", DTask)

    val requiresDOM = SettingKey[Boolean]("requiresDOM",
        "Whether this projects needs the DOM. Overrides anything inherited through dependencies.", AMinusSetting)

    val jsDependencies = SettingKey[Seq[AbstractJSDep]]("jsDependencies",
        "JavaScript libraries this project depends upon. Also used to depend on the DOM.", APlusSetting)

    val jsDependencyFilter = SettingKey[DependencyFilter]("jsDependencyFilter",
        "The filter applied to the raw JavaScript dependencies before execution", CSetting)

    val jsManifestFilter = SettingKey[ManifestFilter]("jsManifestFilter",
        "The filter applied to JS dependency manifests before resolution", CSetting)

    val resolvedJSDependencies = TaskKey[Attributed[Seq[ResolvedJSDependency]]]("resolvedJSDependencies",
        "JS dependencies after resolution.", DTask)

    /** Internal task to calculate whether a project requests the DOM
     *  (through jsDependencies or requiresDOM) */
    val scalaJSRequestsDOM = TaskKey[Boolean]("scalaJSRequestsDOM",
        "Scala.js internal: Whether a project really wants the DOM. " +
        "Calculated using requiresDOM and jsDependencies", KeyRanks.Invisible)

    /** Dummy builder to allow declaractions like:
     *
     *  {{{
     *  RuntimeDOM % "test"
     *  }}}
     */
    val RuntimeDOM = RuntimeDOMDep(None)

    /** Builder to allow declarations like:
     *
     *  {{{
     *  ProvidedJS / "foo.js"
     *  ProvidedJS / "foo.js" % "test"
     *  }}}
     */
    object ProvidedJS {
      def /(name: String): ProvidedJSModuleID = ProvidedJSModuleID(name, None)
    }

    /** Builder to allow declarations like:
     *
     *  {{{
     *  "org.webjars" % "jquery" % "1.10.2" / "jquery.js"
     *  "org.webjars" % "jquery" % "1.10.2" / "jquery.js" % "test"
     *  }}}
     */
    implicit class JSModuleIDBuilder(module: ModuleID) {
      def /(name: String): JarJSModuleID = JarJSModuleID(module, name)
    }

  }

  import autoImport._

  /** Collect certain file types from a classpath.
   *
   *  @param cp Classpath to collect from
   *  @param filter Filter for (real) files of interest (not in jars)
   *  @param collectJar Collect elements from a jar (called for all jars)
   *  @param collectFile Collect a single file. Params are the file and the
   *      relative path of the file (to its classpath entry root).
   *  @return Collected elements attributed with physical files they originated
   *      from (key: scalaJSSourceFiles).
   */
  private def collectFromClasspath[T](cp: Def.Classpath, filter: FileFilter,
      collectJar: VirtualJarFile => Seq[T],
      collectFile: (File, String) => T): Attributed[Seq[T]] = {

    val realFiles = Seq.newBuilder[File]
    val results = Seq.newBuilder[T]

    for (cpEntry <- Attributed.data(cp) if cpEntry.exists) {
      if (cpEntry.isFile && cpEntry.getName.endsWith(".jar")) {
        realFiles += cpEntry
        val vf = new FileVirtualBinaryFile(cpEntry) with VirtualJarFile
        results ++= collectJar(vf)
      } else if (cpEntry.isDirectory) {
        for {
          (file, relPath0) <- Path.selectSubpaths(cpEntry, filter)
        } {
          val relPath = relPath0.replace(java.io.File.separatorChar, '/')
          realFiles += file
          results += collectFile(file, relPath)
        }
      } else {
        sys.error("Illegal classpath entry: " + cpEntry.getPath)
      }
    }

    Attributed.blank(results.result()).put(
        scalaJSSourceFiles, realFiles.result())
  }

  private def jsDependencyManifestsInJar(
      container: VirtualFileContainer): List[JSDependencyManifest] = {
    container.listEntries(_ == JSDependencyManifest.ManifestFileName) {
      (_, stream) =>
        val json = readJSON(new InputStreamReader(stream, "UTF-8"))
        fromJSON[JSDependencyManifest](json)
    }
  }

  /** Concatenates a bunch of VirtualTextFiles to a WritableVirtualTextFile.
   *  Adds a '\n' after each file.
   */
  private def concatFiles(output: WritableVirtualTextFile,
      files: Seq[VirtualTextFile]): Unit = {
    val out = output.contentWriter

    try {
      for (file <- files) {
        toolsIO.writeTo(file, out)
        // New line after each file
        out.write('\n')
      }
    } finally {
      out.close()
    }
  }

  private def packageJSDependenciesSetting(taskKey: TaskKey[File],
      cacheName: String,
      getLib: ResolvedJSDependency => VirtualJSFile): Setting[Task[File]] = {
    taskKey := Def.taskDyn {
      if ((skip in taskKey).value)
        Def.task((artifactPath in taskKey).value)
      else Def.task {
        val s = (streams in taskKey).value
        val deps = resolvedJSDependencies.value
        val output = (artifactPath in taskKey).value

        val realFiles = deps.get(scalaJSSourceFiles).get
        val resolvedDeps = deps.data

        FileFunction.cached(s.cacheDirectory / cacheName,
            FilesInfo.lastModified,
            FilesInfo.exists) { _ => // We don't need the files

          IO.createDirectory(output.getParentFile)

          val outFile = AtomicWritableFileVirtualJSFile(output)
          concatFiles(outFile, resolvedDeps.map(getLib))

          Set(output)
        } (realFiles.toSet)

        output
      }
    }.value
  }

  lazy val configSettings: Seq[Setting[_]] = Seq(
      fastOptJS := fastOptJS.dependsOn(packageJSDependencies).value,
      fullOptJS := fullOptJS.dependsOn(packageJSDependencies).value,
      fullOptJS := fullOptJS.dependsOn(packageMinifiedJSDependencies).value,

      artifactPath in packageJSDependencies :=
        ((crossTarget in packageJSDependencies).value /
            ((moduleName in packageJSDependencies).value + "-jsdeps.js")),

      packageJSDependenciesSetting(packageJSDependencies, "package-js-deps", _.lib),

      artifactPath in packageMinifiedJSDependencies :=
        ((crossTarget in packageMinifiedJSDependencies).value /
            ((moduleName in packageMinifiedJSDependencies).value + "-jsdeps.min.js")),

      packageJSDependenciesSetting(packageMinifiedJSDependencies,
          "package-min-js-deps", dep => dep.minifiedLib.getOrElse(dep.lib)),

      jsDependencyManifest := {
        val myModule = thisProject.value.id
        val config = configuration.value.name

        // Collect all libraries
        val jsDeps = jsDependencies.value.collect {
          case dep: JSModuleID if dep.configurations.forall(_ == config) =>
            dep.jsDep
        }

        val requiresDOM = jsDependencies.value.exists {
          case RuntimeDOMDep(configurations) =>
            configurations.forall(_ == config)
          case _ => false
        }

        val manifest = new JSDependencyManifest(new Origin(myModule, config),
            jsDeps.toList, requiresDOM)

        // Write dependency file to class directory
        val targetDir = classDirectory.value
        IO.createDirectory(targetDir)

        val file = targetDir / JSDependencyManifest.ManifestFileName
        val vfile = WritableFileVirtualTextFile(file)

        // Prevent writing if unnecessary to not invalidate dependencies
        val needWrite = !vfile.exists || {
          Try {
            val readManifest = JSDependencyManifest.read(vfile)
            readManifest != manifest
          } getOrElse true
        }

        if (needWrite)
          JSDependencyManifest.write(manifest, vfile)

        file
      },

      products := products.dependsOn(jsDependencyManifest).value,

      jsDependencyManifests := {
        val filter = jsManifestFilter.value
        val rawManifests = collectFromClasspath(fullClasspath.value,
            new ExactFilter(JSDependencyManifest.ManifestFileName),
            collectJar = jsDependencyManifestsInJar(_),
            collectFile = { (file, _) =>
              fromJSON[JSDependencyManifest](readJSON(IO.read(file)))
            })

        rawManifests.map(manifests => filter(manifests.toTraversable))
      },

      scalaJSNativeLibraries := {
        collectFromClasspath(fullClasspath.value,
            "*.js", collectJar = _.jsFiles,
            collectFile = FileVirtualJSFile.relative)
      },

      resolvedJSDependencies := {
        val dependencyFilter = jsDependencyFilter.value
        val attLibs = scalaJSNativeLibraries.value
        val attManifests = jsDependencyManifests.value

        // Collect originating files
        val realFiles = {
          attLibs.get(scalaJSSourceFiles).get ++
          attManifests.get(scalaJSSourceFiles).get
        }

        // Collect available JS libraries
        val availableLibs = {
          val libs = mutable.Map.empty[String, VirtualJSFile]
          for (lib <- attLibs.data)
            libs.getOrElseUpdate(lib.relativePath, lib)
          libs.toMap
        }

        // Actually resolve the dependencies
        val resolved = DependencyResolver.resolveDependencies(
            attManifests.data, availableLibs, dependencyFilter)

        Attributed.blank[Seq[ResolvedJSDependency]](resolved)
            .put(scalaJSSourceFiles, realFiles)
      },

      // Add the resolved JS dependencies to the list of JS files given to envs
      jsExecutionFiles := {
        val deps = resolvedJSDependencies.value.data

        /* Implement the behavior of commonJSName without having to burn it
         * inside NodeJSEnv, and hence in the JSEnv API.
         * Since this matches against NodeJSEnv specifically, it obviously
         * breaks the OO approach, but oh well ...
         */
        val libs = jsEnv.value match {
          case _: org.scalajs.jsenv.nodejs.NodeJSEnv =>
            val libCache = new VirtualFileMaterializer(false)

            for (dep <- deps) yield {
              dep.info.commonJSName.fold {
                dep.lib
              } { commonJSName =>
                val fname = libCache.materialize(dep.lib).getAbsolutePath
                new MemVirtualJSFile(s"require-$fname").withContent(
                  s"""$commonJSName = require("${escapeJS(fname)}");"""
                )
              }
            }

          case _ =>
            deps.map(_.lib)
        }

        libs ++ jsExecutionFiles.value
      },

      scalaJSRequestsDOM := {
        requiresDOM.?.value.getOrElse(
            jsDependencyManifests.value.data.exists(_.requiresDOM))
      }
  )

  lazy val compileSettings = configSettings

  lazy val testSettings = Def.settings(
      configSettings,

      moduleName in packageJSDependencies := moduleName.value + "-test"
  )

  override def projectSettings: Seq[Setting[_]] = Def.settings(
      inConfig(Compile)(compileSettings),
      inConfig(Test)(testSettings),

      // add all the webjars your jsDependencies depend upon
      libraryDependencies ++= jsDependencies.value.collect {
        case JarJSModuleID(module, _) => module
      },

      jsDependencies := Seq(),
      jsDependencyFilter := identity,
      jsManifestFilter := identity
  )

}
