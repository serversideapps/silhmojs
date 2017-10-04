package scalajsclient.jswidgets

import org.scalajs.dom
import scala.scalajs.js
import scala.scalajs.js.timers._
import scala.scalajs.js.Dynamic.global
import org.singlespaced.d3js.d3
import org.singlespaced.d3js.Ops._
import scalajsclient._
import scalajsclient.ScalaJSUtils._

class JSSubWidget extends JSWidget {
  var scalefactor: Double = 1.0
  def scaled(coord: Double) = scalefactor * coord
  def d3root = d3.select("#" + id)
  def contentid = id + "content"
  def d3content = d3.select("#" + contentid)
  def scaledpx(coord: Double): String = scaled(coord) + "px"
  def aligncenter(boxwidth: Double, contentwidth: Double) = (boxwidth - contentwidth) / 2
  def initdraw {
    d3root.html("")
    d3root.style("position", "relative").style("opacity", "0.99")
    d3root.append("div").attr("id", id + "content").style("position", "relative").style("opacity", "0.99")
  }
  def effective(coord: Double) = scalefactor * coord
  def renderhelper {
    if ((parent != null) && (parent.isInstanceOf[JSContainer])) {
      val peh = parent.effheight
      val pew = parent.effwidth
      scalefactor = peh / TOTAL_HEIGHT
      if (effective(TOTAL_WIDTH) > pew) scalefactor = pew / TOTAL_WIDTH
    }
  }
  def parentup {
    if ((parent != null) && (parent.isInstanceOf[JSContainer])) {
      val p = parent.asInstanceOf[JSContainer]
      p.PR_OP = "1.0"
      p.draw
    }
  }
  def parentdown {
    if ((parent != null) && (parent.isInstanceOf[JSContainer])) {
      val p = parent.asInstanceOf[JSContainer]
      p.PR_OP = "0.99"
      p.draw
    }
  }
}