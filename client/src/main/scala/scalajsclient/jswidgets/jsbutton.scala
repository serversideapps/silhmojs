package scalajsclient.jswidgets

import org.scalajs.dom
import scala.scalajs.js
import scala.scalajs.js.timers._
import scala.scalajs.js.Dynamic.global
import org.singlespaced.d3js.d3
import org.singlespaced.d3js.Ops._
import scalajsclient._
import scalajsclient.ScalaJSUtils._
import JSButton._

object JSButton {
  type ButtonHandler = (JSButton) => Unit
}

class JSButton(
    setid: String,
    label: String = "Action",
    altlabel: String = "Doing Action",
    callback: ButtonHandler = (JSButton) => {},
    width: Double = 150.0,
    height: Double = 25.0
) extends JSSubWidget {
  id = setid
  override def TOTAL_WIDTH = width
  override def TOTAL_HEIGHT = height
  fixedratio = true
  val buttonmargin = height / 12.5
  val buttonheight = height - 2 * buttonmargin
  val buttonwidth = width - 2 * buttonmargin
  def buttonid = id + "button"
  var pushed = false
  val buttonmousedownhandler: MouseHandler = (e: dom.MouseEvent) => {
    if (!pushed) {
      pushed = true
      draw
      callback(this)
    }
  }
  val buttoncolor = "#bfffbf"
  val buttoncolorpressed = "#9fff9f"
  val bckgcolor = "#ffffaf"
  val bckgcolorpressed = "#ffff9f"

  def labelid = id + "label"
  def d3label = d3.select("#" + labelid)

  override def render {
    renderhelper
  }

  val labelfactor = 1.35
  val borderfactor = 15.0

  override def draw {
    initdraw
    parentdown
    if (pushed) {
      d3content.
        style("width", scaledpx(width)).
        style("height", scaledpx(height)).
        style("position", "relative").
        style("background-color", bckgcolor).
        style("opacity", "0.99").
        style("z-index", "50")

      val sbuttonheight = buttonheight - 2 * buttonmargin
      val sbuttonwidth = buttonwidth - 2 * buttonmargin

      val border = sbuttonheight / borderfactor
      val scaledborder = scaled(sbuttonheight / borderfactor)

      d3content.append("div").attr("id", buttonid).
        style("width", scaledpx(sbuttonwidth - 2.0 * border)).
        style("height", scaledpx(sbuttonheight - 2.0 * border)).
        style("position", "absolute").
        style("left", scaledpx(2 * buttonmargin)).
        style("top", scaledpx(2 * buttonmargin)).
        style("border-style", "solid").
        style("border-width", px(scaledborder)).
        style("border-color", "#000000").
        style("cursor", "arrow").
        style("background-color", buttoncolorpressed).
        style("opacity", "1.0").
        style("z-index", "50").
        append("label").attr("id", labelid).
        html(altlabel).
        style("position", "absolute").
        style("font-size", scaledpx(sbuttonheight / labelfactor)).
        style("opacity", "0.99").
        style("z-index", "51")

      d3label.
        style("left", px(aligncenter(scaled(sbuttonwidth) + 2 * scaledborder, gbcrwidthbyid(labelid)))).
        style("top", px(aligncenter(scaled(sbuttonheight) - 2 * scaledborder, gbcrheightbyid(labelid)))).
        style("cursor", "pointer").
        style("padding", "0px").
        style("margin", "0px").
        style("opacity", "0.99").
        style("z-index", "50")
    } else {
      d3content.
        style("width", scaledpx(width)).
        style("height", scaledpx(height)).
        style("position", "absolute").
        style("background-color", bckgcolor).
        style("opacity", "0.99").
        style("z-index", "50")

      val border = buttonheight / borderfactor
      val scaledborder = scaled(buttonheight / borderfactor)

      d3content.append("div").attr("id", buttonid).
        style("width", scaledpx(buttonwidth - 2.0 * border)).
        style("height", scaledpx(buttonheight - 2.0 * border)).
        style("position", "absolute").
        style("left", scaledpx(buttonmargin)).
        style("top", scaledpx(buttonmargin)).
        style("border-style", "solid").
        style("border-width", px(scaledborder)).
        style("border-color", "#9f9f9f").
        style("cursor", "pointer").
        style("background-color", buttoncolor).
        style("opacity", "1.0").
        style("z-index", "50").
        append("label").attr("id", labelid).
        html(label).
        style("position", "absolute").
        style("font-size", scaledpx(buttonheight / labelfactor)).
        style("opacity", "0.99").
        style("z-index", "51")

      d3label.
        style("left", px(aligncenter(scaled(buttonwidth) + 2 * scaledborder, gbcrwidthbyid(labelid)))).
        style("top", px(aligncenter(scaled(buttonheight) - 2 * scaledborder, gbcrheightbyid(labelid)))).
        style("cursor", "pointer").
        style("padding", "0px").
        style("margin", "0px").
        style("opacity", "0.99").
        style("z-index", "50")

      dgebid(buttonid).onmousedown = buttonmousedownhandler
    }
  }
}