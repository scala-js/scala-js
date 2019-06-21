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

package org.scalajs.core.tools.jsdep

import CollectionsCompat.TraversableCompat

/** Holds useful JSDependencyManifest filters */
object ManifestFilters {

  type ManifestFilter =
    TraversableCompat[JSDependencyManifest] => TraversableCompat[JSDependencyManifest]

  /** Creates a manifest filter that maps resource names of a certain
   *  origin as if they were written differently
   *  @param moduleNames Modules for which the mapping should be applied
   *  @param nameMappings resource name mappings
   */
  def reinterpretResourceNames(moduleNames: String*)(
      nameMappings: (String, String)*): ManifestFilter = {
    val modSet = moduleNames.toSet
    val nameMap = nameMappings.toMap

    val mapper = { (origin: Origin) => (oldName: String) =>
      if (modSet(origin.moduleName))
        nameMap.getOrElse(oldName, oldName)
      else
        oldName
    }

    reinterpretResourceNames(mapper)
  }

  /** Creates a manifest filter that maps resource names of a certain
   *  origin as if they were written differently
   *  @param mappings Maps manifest origin to old resource name to new
   *      resource name
   */
  def reinterpretResourceNames(
      mappings: Origin => String => String): ManifestFilter = { manifests =>
    for (manifest <- manifests) yield {
      val mapping = mappings(manifest.origin)
      val filteredJSDeps = for (jsDependency <- manifest.libDeps)
        yield new JSDependency(
            mapping(jsDependency.resourceName),
            jsDependency.dependencies.map(mapping),
            jsDependency.commonJSName,
            jsDependency.minifiedResourceName.map(mapping))
      new JSDependencyManifest(manifest.origin, filteredJSDeps,
          manifest.requiresDOMInternal, manifest.compliantSemantics,
          internal = ())
    }
  }
}
