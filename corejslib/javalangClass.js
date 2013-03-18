/* ------------------
 * java.lang.Class
 * ------------------ */

(function ($env) {
  var ObjectClass = $env.classes["java.lang.Object"].type;

  function ClassClass() {
    ObjectClass.prototype.constructor.call(this);
    this.$data = null;
  }
  ClassClass.prototype = Object.create(ObjectClass.prototype);
  ClassClass.prototype.constructor = ClassClass;

  ClassClass.prototype["<init>(<special>):java.lang.Class"] = function(data) {
    ObjectClass.prototype["<init>():java.lang.Object"].call(this);
    this.$data = data;
    return this;
  }

  ClassClass.prototype["getName():java.lang.String"] = function() {
    return this.$data.displayName;
  }

  ClassClass.prototype.getName = ClassClass.prototype["getName():java.lang.String"];

  ClassClass.prototype["toString():java.lang.String"] = function() {
    var displayName = this.$data.displayName;
    if (this.$data.isInterface)
      return "interface "+displayName;
    else if (!this.$data.isPrimitive)
      return "class "+displayName;
    else
      return displayName;
  }

  ClassClass.prototype["isInstance(java.lang.Object):scala.Boolean"] = function(obj) {
    return $env.isInstance(obj, this.$data.name);
  }

  ClassClass.prototype["isAssignableFrom(java.lang.Class):scala.Boolean"] = function(subClass) {
    return subClass.$data.ancestors[this.$data.name] ? true : false;
  }

  ClassClass.prototype["getSuperClass():java.lang.Class"] = function() {
    return this.$data.parentData === null ? null : this.$data.parentData.class;
  }

  ClassClass.prototype["isPrimitive():scala.Boolean"] = function() {
    return this.$data.isPrimitive;
  }

  ClassClass.prototype["isArray():scala.Boolean"] = function() {
    return this.$data.isArray;
  }

  ClassClass.prototype["getComponentType():java.lang.Class"] = function() {
    return this.$data.isArray ? this.$data.componentData.class : null;
  }

  $env.createClass("java.lang.Class", ClassClass, "java.lang.Object", {
    "java.lang.Object": true,
    "java.lang.Class": true
  });
})($ScalaJSEnvironment);
