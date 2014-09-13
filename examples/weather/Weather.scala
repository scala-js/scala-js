package com.knoldus.weather

import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js
import js.Dynamic.{ global => g, newInstance => jsnew, literal => lit }
import org.scalajs.dom
import dom.document
import org.scalajs.dom.XMLHttpRequest
import scala.scalajs.js.JSON
import scala.scalajs.js.Date

object Weather extends js.JSApp {

  def main(): Unit = {
    println("---------Weather Information System---------------")
  }

  @JSExport
  def showDetail() {

    val name = g.document.getElementById("name").value
    val xmlHttpRequest = new dom.XMLHttpRequest

    xmlHttpRequest.open("GET", "http://api.openweathermap.org/data/2.5/weather?q=" + name, false)
    xmlHttpRequest.send(null);
    val result = JSON.parse(xmlHttpRequest.responseText)

    if (result.cod.toString() == "404") {
      g.alert("Please Enter A Valid City Name.")
    } else {
      val weather = result.weather.asInstanceOf[js.Array[js.Dynamic]](0)
      document.getElementById("tempDetail").style.display = "block"
      document.getElementById("cityName").innerHTML = result.name + "," + result.sys.country
      val image = "http://openweathermap.org/img/w/" + weather.icon + ".png"
      document.getElementById("temp").innerHTML = "<img src=" + image + " >" + (result.main.temp - 273.15)
      document.getElementById("weather").innerHTML = "" + weather.main
      document.getElementById("pressure").innerHTML = "" + result.main.pressure + " hpa"
      document.getElementById("humidity").innerHTML = result.main.humidity + " %"
      document.getElementById("sunrise").innerHTML = msToTime(result.sys.sunrise.toString.toLong)
      document.getElementById("sunset").innerHTML = msToTime(result.sys.sunset.toString.toLong)
      document.getElementById("geocoords").innerHTML = "[" + result.coord.lon + ", " + result.coord.lat + "]"
      initialize(result.coord.lat.toString.toDouble, result.coord.lon.toString.toDouble)
    }
  }

  def initialize(lat: Double, long: Double) = {
    val map_canvas = document.getElementById("map_canvas")
    val map_options = lit(center = (jsnew(g.google.maps.LatLng))(lat, long), zoom = 3, mapTypeId = g.google.maps.MapTypeId.ROADMAP)
    val gogleMap = (jsnew(g.google.maps.Map))(map_canvas, map_options)
    val marker = (jsnew(g.google.maps.Marker))(lit(map = gogleMap, position = (jsnew(g.google.maps.LatLng)(lat, long))))
  }

  def msToTime(unix_timestamp: Long) = {
    val date = new Date(unix_timestamp * 1000);
    val hrs = date.getHours();
    val mins = date.getMinutes();
    val secs = date.getSeconds();
    hrs + ":" + mins + ":" + secs
  }

}