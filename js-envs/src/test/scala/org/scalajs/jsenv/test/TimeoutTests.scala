package org.scalajs.jsenv.test

import org.junit.Test
import org.junit.Assert._

import scala.concurrent.duration._

trait TimeoutTests extends JSEnvTest {

  @Test
  def basicTimeoutTest = {

    val deadline = 300.millis.fromNow

    """
    setTimeout(function() { console.log("1"); }, 200);
    setTimeout(function() { console.log("2"); }, 100);
    setTimeout(function() { console.log("3"); }, 300);
    setTimeout(function() { console.log("4"); },   0);
    """ hasOutput
    """|4
       |2
       |1
       |3
       |""".stripMargin

    assertTrue("Execution took too little time", deadline.isOverdue())

  }

  @Test
  def clearTimeoutTest = {

    val deadline = 300.millis.fromNow

    """
    var c = setTimeout(function() { console.log("1"); }, 200);
    setTimeout(function() {
      console.log("2");
      clearTimeout(c);
    }, 100);
    setTimeout(function() { console.log("3"); }, 300);
    setTimeout(function() { console.log("4"); },   0);
    """ hasOutput
    """|4
       |2
       |3
       |""".stripMargin

    assertTrue("Execution took too little time", deadline.isOverdue())

  }

  @Test
  def timeoutArgTest = {

    val deadline = 300.millis.fromNow

    """
    setTimeout(function(a, b) { console.log("1" + a + b); }, 200, "foo", "bar");
    setTimeout(function() { console.log("2"); }, 100);
    setTimeout(function(msg) { console.log(msg); }, 300, "Hello World");
    setTimeout(function() { console.log("4"); },   0);
    """ hasOutput
    """|4
       |2
       |1foobar
       |Hello World
       |""".stripMargin

    assertTrue("Execution took too little time", deadline.isOverdue())

  }

  @Test
  def intervalTest = {

    val deadline = 1.second.fromNow

    """
    var i1 = setInterval(function() { console.log("each 203"); }, 203);
    var i2 = setInterval(function() { console.log("each 301"); }, 301);
    var i3 = setInterval(function() { console.log("each 103"); }, 103);

    setTimeout(function() {
      clearInterval(i1);
      clearInterval(i2);
      clearInterval(i3);
    }, 1000);
    """ hasOutput
    """|each 103
       |each 203
       |each 103
       |each 301
       |each 103
       |each 203
       |each 103
       |each 103
       |each 301
       |each 203
       |each 103
       |each 103
       |each 203
       |each 103
       |each 301
       |each 103
       |""".stripMargin

     assertTrue("Execution took too little time", deadline.isOverdue())

  }

  @Test
  def intervalSelfClearTest = {

    val deadline = 100.millis.fromNow

    """
    var c = 0;
    var i = setInterval(function() {
      c++;
      console.log(c.toString());
      if (c >= 10)
        clearInterval(i);
    }, 10);
    """ hasOutput (1 to 10).map(_ + "\n").mkString

    assertTrue("Execution took too little time", deadline.isOverdue())

  }

}
