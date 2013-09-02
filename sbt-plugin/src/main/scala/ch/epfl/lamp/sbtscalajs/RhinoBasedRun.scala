package ch.epfl.lamp.sbtscalajs

import sbt._

import scala.collection.mutable

import org.mozilla.javascript._

object RhinoBasedRun {

  private class ContextOps(val self: Context) {
    def evaluateFile(scope: Scriptable, file: File,
        securityDomain: AnyRef = null): Any = {
      val reader = new java.io.FileReader(file)
      try {
        self.evaluateReader(scope, reader,
            file.getAbsolutePath, 1, securityDomain)
      } finally {
        reader.close()
      }
    }
  }

  private implicit def context2ops(ctx: Context): ContextOps =
    new ContextOps(ctx)

  /** A proxy for a Logger that looks like a Mozilla console object */
  private class LoggingConsole(logger: Logger) {
    def log(x: Any): Unit = logger.info(x.toString)
    def info(x: Any): Unit = logger.info(x.toString)
    def warn(x: Any): Unit = logger.warn(x.toString)
    def error(x: Any): Unit = logger.error(x.toString)
  }

  /** A proxy for a ScalaJS "scope" field that loads scripts lazily
   *
   *  E.g., ScalaJS.c, which is a scope with the Scala.js classes, can be
   *  turned to a LazyScalaJSScope. Upon first access to a field of ScalaJS.c,
   *  say ScalaJS.c.scala\ufe33Option, the script defining that particular
   *  field will be loaded.
   *  This is possible because the relative path to the script can be derived
   *  from the name of the property being accessed.
   *
   *  It is immensely useful, because it allows to load lazily only the scripts
   *  that are actually needed.
   */
  private class LazyScalaJSScope(
      providers: scala.collection.Map[String, File],
      globalScope: Scriptable,
      base: Scriptable,
      isModule: Boolean = false,
      isTraitImpl: Boolean = false)
  extends Scriptable {

    private val fields = mutable.HashMap.empty[String, Any]
    private var prototype: Scriptable = _
    private var parentScope: Scriptable = _

    {
      // Pre-fill fields with the properties of `base`
      for (id <- base.getIds()) {
        (id.asInstanceOf[Any]: @unchecked) match {
          case name: String => put(name, this, base.get(name, base))
          case index: Int => put(index, this, base.get(index, base))
        }
      }
    }

    private def load(name: String): Unit = {
      val relativeFileName = nameToRelativeFileName(name)
      providers.get(relativeFileName) foreach { file =>
        val ctx = Context.getCurrentContext()
        ctx.evaluateFile(globalScope, file)
      }
    }

    private def nameToRelativeFileName(name0: String): String = {
      val name = name0.replace("\ufe33", "/")
      if (isModule) name + "$.js"
      else if (!isTraitImpl) name + ".js"
      else name.split("\ufe34")(0) + ".js"
    }

    override def getClassName() = "LazyScalaJSScope"

    override def get(name: String, start: Scriptable) = {
      fields.getOrElse(name, {
        load(name)
        fields.getOrElse(name, Scriptable.NOT_FOUND)
      }).asInstanceOf[AnyRef]
    }
    override def get(index: Int, start: Scriptable) =
      get(index.toString, start)

    override def has(name: String, start: Scriptable) =
      fields.contains(name)
    override def has(index: Int, start: Scriptable) =
      has(index.toString, start)

    override def put(name: String, start: Scriptable, value: Any) = {
      fields(name) = value
    }
    override def put(index: Int, start: Scriptable, value: Any) =
      put(index.toString, start, value)

    override def delete(name: String) = ()
    override def delete(index: Int) = ()

    override def getPrototype() = prototype
    override def setPrototype(value: Scriptable) = prototype = value

    override def getParentScope() = parentScope
    override def setParentScope(value: Scriptable) = parentScope = value

    override def getIds() = fields.keys.toArray

    override def getDefaultValue(hint: java.lang.Class[_]) = {
      val arg = if (hint eq null) "undefined" else hint.getName
      throw ScriptRuntime.typeError1("msg.default.value", arg)
    }

    override def hasInstance(instance: Scriptable) = false
  }

  /** Run a sequence of JavaScript scripts, with a Scala.js flavor
   *  If given the original Scala.js classpath, it can hijack the Scala.js
   *  scripts in the sequence to load them lazily, the first time they are
   *  required.
   */
  def scalaJSRunJavaScript(logger: Logger, inputs: Seq[File],
      useLazyScalaJSScopes: Boolean = false,
      scalaJSClasspath: Seq[File] = Nil): Unit = {
    val ctx = Context.enter()
    try {
      val scope = ctx.initStandardObjects()

      ScriptableObject.putProperty(scope, "console",
          Context.javaToJS(new LoggingConsole(logger), scope))

      if (!useLazyScalaJSScopes ||
          !inputs.exists(_.getName == "scalajs-corejslib.js")) {
        // Easy, just evaluate all input files, in order
        for (input <- inputs)
          ctx.evaluateFile(scope, input)
      } else {
        // The smart thing that hijacks ScalaJS-related things
        runJavaScriptWithLazyScalaJSScopes(ctx, scope, inputs, scalaJSClasspath)
      }
    } finally {
      Context.exit()
    }
  }

  private def runJavaScriptWithLazyScalaJSScopes(ctx: Context,
      scope: Scriptable, inputs: Seq[File],
      scalaJSClasspath: Seq[File]): Unit = {

    val providers = mutable.HashMap.empty[String, File]
    val ScalaJSProvider = """[0-9]{4}-(.*\.js)""".r

    for (input <- inputs) {
      input.getName match {
        case "scalajs-corejslib.js" =>
          ctx.evaluateFile(scope, input)

          // Hijack the global ScalaJS instance
          val ScalaJS = scope.get("ScalaJS", scope).asInstanceOf[Scriptable]

          def patchField(name: String, isModule: Boolean = false,
              isTraitImpl: Boolean = false): Unit = {
            val base = ScalaJS.get(name, ScalaJS).asInstanceOf[Scriptable]
            val lazyfied = new LazyScalaJSScope(providers, scope, base,
                isModule, isTraitImpl)
            ScalaJS.put(name, ScalaJS, lazyfied)
          }

          patchField("data")
          patchField("c")
          patchField("inheritable")
          patchField("classes")
          patchField("impls", isTraitImpl = true)
          patchField("moduleInstances", isModule = true)
          patchField("modules", isModule = true)
          patchField("is")
          patchField("as")
          patchField("isArrayOf")
          patchField("asArrayOf")

        case ScalaJSProvider(fileName) =>
          val relative = scalaJSClasspath collectFirst {
            case base if IO.relativize(base, input).isDefined =>
              IO.relativize(base, input).get
          }

          if (relative.isDefined) {
            val rel = relative.get.replace("\\", "/")
            val lastSlash = rel.lastIndexOf("/")
            val relativeFileName = rel.substring(0, lastSlash+1) + fileName
            providers += (relativeFileName -> input)
          } else {
            // Oops! We found a Scala.js file that is not in the classpath!
            ctx.evaluateFile(scope, input)
          }

        case _ =>
          ctx.evaluateFile(scope, input)
      }
    }
  }
}
