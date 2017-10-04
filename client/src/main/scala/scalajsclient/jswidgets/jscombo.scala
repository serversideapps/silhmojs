package scalajsclient.jswidgets

import org.scalajs.dom
import scala.scalajs.js
import scala.scalajs.js.timers._
import scala.scalajs.js.Dynamic.global
import org.singlespaced.d3js.d3
import org.singlespaced.d3js.Ops._
import scalajsclient._
import scalajsclient.ScalaJSUtils._

class JSCombo(
    setid: String,
    options: List[String],
    selected: Option[String] = None,
    width: Double = 150.0,
    height: Double = 25.0,
    selectedcallback: (String) => Unit = (s: String) => {}
) extends JSSubWidget {
  id = setid
  override def TOTAL_WIDTH = width
  override def TOTAL_HEIGHT = height
  var current: String = null
  if (!selected.isEmpty) current = selected.get else if (options.length > 0) current = options(0)
  fixedratio = true
  def getselected: String =
    {
      if (current == null)
        if (selected.isEmpty)
          if (options.length > 0) options(0) else ""
        else selected.get
      else current
    }
  def setselected(sel: String) {
    current = sel
    open = false
    draw
  }
  def labelid = id + "label"
  val buttonmargin = height / 12.5
  val buttonheight = height - buttonmargin * 2
  val buttonwidth = buttonheight
  def selwidth = width - buttonwidth - 3 * buttonmargin
  def buttonid = id + "button"
  var open = false
  def optwidth = selwidth
  def optid(i: Int) = id + "opt" + i
  val buttonmousedownhandler: MouseHandler = (e: dom.MouseEvent) => {
    open = !open
    draw
  }

  val openmouseouthandler: MouseHandler = (e: dom.MouseEvent) => {
    if (!isincrid(e.clientX, e.clientY, contentid)) {
      open = false
      draw
    }
  }
  val optmouseouthandler: MouseHandler = (e: dom.MouseEvent) => {
  }
  def coloropts(seli: Int) {
    var i = 0
    for (option <- options) {
      d3.select("#" + optid(i)).
        style("background-color", if (i == seli) selcolor else "#dfffaf")
      i += 1
    }
  }
  def optmouseinhandler(seli: Int): MouseHandler = (e: dom.MouseEvent) => {
    coloropts(seli)
  }
  def optmousedownhandler(seli: Int): MouseHandler = (e: dom.MouseEvent) => {
    current = options(seli)
    open = false
    selectedcallback(current)
    draw
  }
  def getselectedindex(sel: String): Int =
    {
      options.indexOf(sel)
    }
  override def render {
    renderhelper
  }
  val selcolor = "#bfbfff"
  val buttoncolor = "#bfffbf"
  val buttoncolorpressed = "#9fff9f"
  val optscolor = "#ffffaf"
  val optscolorpressed = "#ffff9f"

  def FONT_MARGIN = buttonheight / 8.0
  def FONT_SIZE = FONT_MARGIN * 6.0
  val border = buttonheight / 15.0

  override def draw {
    parentdown
    initdraw
    if (open) {
      parentup
      d3content.
        style("width", scaledpx(width)).
        style("height", scaledpx(options.length * height)).
        style("background-color", optscolorpressed).
        style("position", "relative").
        style("opacity", "1.0").
        style("z-index", "55")

      d3content.append("div").attr("id", buttonid).
        style("width", scaledpx(buttonwidth - 4 * buttonmargin)).
        style("height", scaledpx(buttonheight - 4 * buttonmargin)).
        style("position", "absolute").
        style("left", scaledpx(selwidth + 5 * buttonmargin - border)).
        style("top", scaledpx(2 * buttonmargin)).
        style("border-style", "solid").
        style("border-width", scaledpx(border)).
        style("cursor", "pointer").
        style("background-color", buttoncolorpressed).
        style("opacity", "1.0").
        style("z-index", "55")

      dgebid(buttonid).onmousedown = buttonmousedownhandler

      dgebid(contentid).onmouseout = openmouseouthandler
      var i = 0
      for (option <- options) {
        d3content.append("div").attr("id", optid(i)).
          style("width", scaledpx(optwidth)).
          style("height", scaledpx(buttonheight)).
          style("position", "absolute").
          style("top", scaledpx(i * height + buttonmargin)).
          style("left", scaledpx(buttonmargin)).
          style("opacity", "1.0").
          style("z-index", "60").
          append("label").attr("id", id + "label" + i).
          style("position", "absolute").
          style("top", scaledpx(FONT_MARGIN)).
          style("cursor", "pointer").
          style("font-size", scaledpx(FONT_SIZE)).
          style("z-index", "55").
          html(options(i))

        val lid = id + "label" + i
        d3.select("#" + lid).
          style("left", px(aligncenter(scaled(selwidth), gbcrwidthbyid(lid))))

        dgebid(optid(i)).onmouseout = optmouseouthandler
        dgebid(optid(i)).onmouseenter = optmouseinhandler(i)
        dgebid(optid(i)).onmousedown = optmousedownhandler(i)
        i += 1
      }
      coloropts(getselectedindex(current))

      d3content.style("transform", "scale(0.3,0.3)")

      d3content.transition().
        style("transform", "scale(1,1)")
    } else {
      d3content.
        style("width", scaledpx(width)).
        style("height", scaledpx(height)).
        style("position", "relative").
        style("background-color", optscolor).
        style("opacity", "0.99").
        style("z-index", "50")

      d3content.append("div").attr("id", buttonid).
        style("width", scaledpx(buttonwidth - buttonmargin - border)).
        style("height", scaledpx(buttonheight - buttonmargin - border)).
        style("position", "absolute").
        style("left", scaledpx(selwidth + 2 * buttonmargin + border)).
        style("top", scaledpx(buttonmargin)).
        style("cursor", "pointer").
        style("background-color", buttoncolor).
        style("border-style", "solid").
        style("border-color", "#9f9f9f").
        style("border-width", scaledpx(border)).
        style("opacity", "0.99").
        style("z-index", "50")

      d3content.append("div").attr("id", id + "sel").
        style("width", scaledpx(selwidth)).
        style("height", scaledpx(buttonheight)).
        style("top", scaledpx(buttonmargin)).
        style("left", scaledpx(buttonmargin)).
        style("position", "relative").
        style("background-color", selcolor).
        style("z-index", "50").
        append("label").attr("id", id + "label").
        style("position", "absolute").
        style("top", scaledpx(FONT_MARGIN)).
        style("cursor", "pointer").
        style("font-size", scaledpx(FONT_SIZE)).
        style("z-index", "50").
        html(getselected)

      d3.select("#" + id + "label").
        style("left", px(aligncenter(scaled(selwidth), gbcrwidthbyid(labelid))))

      dgebid(buttonid).onmousedown = buttonmousedownhandler
      dgebid(id + "sel").onmousedown = buttonmousedownhandler
    }
  }
}