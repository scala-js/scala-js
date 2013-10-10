/* Scala.js example code
 * Public domain
 * Author: Sébastien Doeraene
 */

$(function() {
  var mainInstance = new ScalaJS.classes.reversi\ufe33Reversi(
    jQuery, jQuery("#playground"));
  mainInstance.startGame();
});
