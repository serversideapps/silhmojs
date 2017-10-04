package scalajsclient

import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.timers._
import scala.scalajs.js.Dynamic.global

import org.singlespaced.d3js.d3
import org.singlespaced.d3js.Ops._

import smartchess._

import shared._
import ScalaJSUtils._

import upickle._
import upickle.default._

//import akka.actor._

class JSWidget() {
  var id: String = null // has to be null for scalajs
  var parent: JSContainer = null // has to be null for scalajs
  def draw {}
  def render {}
  def hide {}
  def show {}
  def TOTAL_WIDTH: Double = 100.0
  def TOTAL_HEIGHT: Double = 100.0
  var fixedratio = false
}

object JSContainer {
  val HANDLE_WIDTH: Double = 10.0
  val PLUS_MARGIN: Double = 20.0
}

case class ViewActorJS() extends SocketActorJS {
  def receive = {
    case SocketOpenedMsg =>
    case x: StoreViewMessage => sendMsg(x)
    case _ => println("This was unexpected.")
  }
}

/*case class ViewActor() extends SocketActor {
  def receive = {
    case SocketOpenedMsg =>
    case x: StoreViewMessage => sendMsg(x)
    case _ => println("This was unexpected.")
  }
}*/

object JSView {
}

case class JSContainerSer(
  top: Double,
  left: Double,
  width: Double,
  height: Double
)

case class JSViewSer(
  containers: Map[String, JSContainerSer]
)

class JSContainer(
    setwidget: JSWidget,
    settop: Double = 0.0,
    setleft: Double = 0.0,
    setwidth: Double = 100.0,
    setheight: Double = 100.0,
    snap: Boolean = false
) {
  import JSContainer._
  // init to null for scalajs, ??? causes error
  var id: String = null
  var parent: JSView = null

  var top: Double = 0.0
  var left: Double = 0.0
  var width: Double = 0.0
  var height: Double = 0.0
  def bottom: Double = top + height
  def right: Double = left + width
  var scalefactor: Double = 1.0

  def raweffheight = height - 2 * HANDLE_WIDTH
  def effheight = scaled(raweffheight)
  def raweffwidth = width - 2 * HANDLE_WIDTH
  def effwidth = scaled(raweffwidth)

  val widget = setwidget
  widget.parent = this

  top = settop
  left = setleft
  width = setwidth
  height = setheight

  if (snap) {
    width = widget.TOTAL_WIDTH + 2 * HANDLE_WIDTH
    height = widget.TOTAL_HEIGHT + 2 * HANDLE_WIDTH
  }

  def me = s(id)
  def mee = dgebid(id)
  def metop = gbcrtopd(mee)
  def meleft = gbcrleftd(mee)

  var abstop: Double = 0.0
  var absleft: Double = 0.0

  var origtop: Double = 0.0
  var origleft: Double = 0.0
  var origheight: Double = 0.0
  var origwidth: Double = 0.0

  var toprightdragunderway = false
  var topleftdragunderway = false
  var bottomrightdragunderway = false
  var bottomleftdragunderway = false

  var offsetx: Double = 0.0
  var offsety: Double = 0.0
  var dragunderway: Boolean = false

  def scaled(coord: Double) = scalefactor * coord

  def topelement = dgebid(id + "top")
  def topleftelement = dgebid(id + "topleft")
  def bottomleftelement = dgebid(id + "bottomleft")
  def leftelement = dgebid(id + "left")
  def bottomelement = dgebid(id + "bottom")
  def toprightelement = dgebid(id + "topright")
  def bottomrightelement = dgebid(id + "bottomright")
  def rightelement = dgebid(id + "right")

  def serialize: String = "%.2f %.2f %.2f %.2f".format(top, left, width, height)

  def upickleSer = JSContainerSer(top, left, width, height)

  def upickleDeSer(s: JSContainerSer) {
    top = s.top
    left = s.left
    width = s.width
    height = s.height
  }

  def deserialize(content: String) {
    val parts = content.split(" ").toList
    top = parts(0).toDouble; left = parts(1).toDouble
    width = parts(2).toDouble; height = parts(3).toDouble
  }

  def generaldragstart {
    widget.hide
  }

  def fixratio {
    if (widget.fixedratio) {
      if ((raweffwidth / widget.TOTAL_WIDTH) != (raweffheight / widget.TOTAL_HEIGHT)) {
        if (raweffwidth > raweffheight) height = ((raweffwidth * widget.TOTAL_HEIGHT / widget.TOTAL_WIDTH) + 2 * HANDLE_WIDTH)
        else width = ((raweffheight * widget.TOTAL_WIDTH / widget.TOTAL_HEIGHT) + 2 * HANDLE_WIDTH)
      }
    }
  }

  def generaldragend {
    parent.tofront(id)
    fixratio
    parent.buildRenderAndDraw
    widget.show
  }

  def startnormaldrag(x: Double, y: Double) {
    if (!dragunderway) {
      offsetx = (meleft - x); offsety = (metop - y)
      me.style("z-index", "100")
      dragunderway = true
      generaldragstart
    }
  }

  val dragstarthandler: DragHandler = (de: dom.raw.HTMLElement, e: dom.DragEvent) => {
    e.preventDefault(); startnormaldrag(e.clientX, e.clientY); draw
  }

  def startdrag(x: Double, y: Double) {
    offsetx = (meleft - x); offsety = (metop - y)
    origtop = top; origleft = left
    origheight = height; origwidth = width
    me.style("z-index", "100")
    generaldragstart
  }

  def toprightstartdrag(x: Double, y: Double) { startdrag(x, y); toprightdragunderway = true; draw }
  def topleftstartdrag(x: Double, y: Double) { startdrag(x, y); topleftdragunderway = true; draw }
  def bottomleftstartdrag(x: Double, y: Double) { startdrag(x, y); bottomleftdragunderway = true; draw }
  def bottomrightstartdrag(x: Double, y: Double) { startdrag(x, y); bottomrightdragunderway = true; draw }

  val toprightdragstarthandler: DragHandler = (de: dom.raw.HTMLElement, e: dom.DragEvent) => {
    e.preventDefault(); toprightstartdrag(e.clientX, e.clientY)
  }
  val bottomrightdragstarthandler: DragHandler = (de: dom.raw.HTMLElement, e: dom.DragEvent) => {
    e.preventDefault(); bottomrightstartdrag(e.clientX, e.clientY)
  }
  val topleftdragstarthandler: DragHandler = (de: dom.raw.HTMLElement, e: dom.DragEvent) => {
    e.preventDefault(); topleftstartdrag(e.clientX, e.clientY)
  }
  val bottomleftdragstarthandler: DragHandler = (de: dom.raw.HTMLElement, e: dom.DragEvent) => {
    e.preventDefault(); bottomleftstartdrag(e.clientX, e.clientY)
  }

  val toprightmousemovehandler: MouseHandler = (e: dom.MouseEvent) => {
    if (toprightdragunderway) {
      abstop = e.clientY - roottop + offsety; absleft = e.clientX - rootleft + offsetx
      val newtop = abstop / scalefactor; val newleft = absleft / scalefactor
      val dwidth = left - newleft; val dheight = origtop - newtop
      width = origwidth - dwidth; height = origheight + dheight; top = newtop
      draw
    }
  }

  val bottomrightmousemovehandler: MouseHandler = (e: dom.MouseEvent) => {
    if (bottomrightdragunderway) {
      abstop = e.clientY - roottop + offsety; absleft = e.clientX - rootleft + offsetx
      val newtop = abstop / scalefactor; val newleft = absleft / scalefactor
      val dwidth = left - newleft; val dheight = newtop - top
      width = origwidth - dwidth; height = origheight + dheight;
      draw
    }
  }

  val topleftmousemovehandler: MouseHandler = (e: dom.MouseEvent) => {
    if (topleftdragunderway) {
      abstop = e.clientY - roottop + offsety
      absleft = e.clientX - rootleft + offsetx
      val newtop = abstop / scalefactor; val newleft = absleft / scalefactor
      val dwidth = left - newleft; val dheight = top - newtop
      width = width + dwidth; height = height + dheight
      left = newleft; top = newtop
      draw
    }
  }

  val bottomleftmousemovehandler: MouseHandler = (e: dom.MouseEvent) => {
    if (bottomleftdragunderway) {
      abstop = e.clientY - roottop + offsety
      absleft = e.clientX - rootleft + offsetx
      val newtop = abstop / scalefactor; val newleft = absleft / scalefactor
      val dwidth = left - newleft; val dheight = top - newtop
      width = width + dwidth; height = origheight - dheight
      left = newleft;
      draw
    }
  }

  val toprightmousedownhandler: MouseHandler = (e: dom.MouseEvent) => {
    toprightstartdrag(e.clientX, e.clientY)
  }
  val bottomrightmousedownhandler: MouseHandler = (e: dom.MouseEvent) => {
    bottomrightstartdrag(e.clientX, e.clientY)
  }
  val topleftmousedownhandler: MouseHandler = (e: dom.MouseEvent) => {
    topleftstartdrag(e.clientX, e.clientY)
  }
  val bottomleftmousedownhandler: MouseHandler = (e: dom.MouseEvent) => {
    bottomleftstartdrag(e.clientX, e.clientY)
  }

  val toprightmouseuphandler: MouseHandler = (e: dom.MouseEvent) => {
    if (toprightdragunderway) {
      me.style("z-index", "50")
      toprightdragunderway = false
      generaldragend
    }
  }

  val bottomrightmouseuphandler: MouseHandler = (e: dom.MouseEvent) => {
    if (bottomrightdragunderway) {
      me.style("z-index", "50")
      bottomrightdragunderway = false
      generaldragend
    }
  }

  val topleftmouseuphandler: MouseHandler = (e: dom.MouseEvent) => {
    if (topleftdragunderway) {
      me.style("z-index", "50")
      topleftdragunderway = false
      generaldragend
    }
  }

  val bottomleftmouseuphandler: MouseHandler = (e: dom.MouseEvent) => {
    if (bottomleftdragunderway) {
      me.style("z-index", "50")
      bottomleftdragunderway = false
      generaldragend
    }
  }

  val mousemovehandler: MouseHandler = (e: dom.MouseEvent) => {
    if (dragunderway) {
      abstop = e.clientY - roottop + offsety
      absleft = e.clientX - rootleft + offsetx
      me.style("left", absleft + "px").style("top", abstop + "px")
    }
    val dummy = me
  }

  val mousedownhandler: MouseHandler = (e: dom.MouseEvent) => {
    startnormaldrag(e.clientX, e.clientY); draw
  }

  val mouseuphandler: MouseHandler = (e: dom.MouseEvent) => {
    if (dragunderway) {
      mousemovehandler(e)
      me.style("z-index", "50")
      top = abstop / scalefactor; left = absleft / scalefactor
      dragunderway = false
      generaldragend
    }
  }

  val CORNER_COLOR = "#afafaf"
  val BAR_COLOR = "#bfbfbf"
  val OPACITY = "0.5"
  var PR_OP = "0.99"

  def draw {
    val dd = if (dragunderway) HANDLE_WIDTH else 0.0
    val dtl = if (topleftdragunderway) HANDLE_WIDTH else 0.0
    val dbl = if (bottomleftdragunderway) HANDLE_WIDTH else 0.0
    val dtr = if (toprightdragunderway) HANDLE_WIDTH else 0.0
    val dbr = if (bottomrightdragunderway) HANDLE_WIDTH else 0.0
    me.
      style("top", scaled(top) + "px").
      style("left", scaled(left) + "px").
      style("width", scaled(width * 0.0) + "px").
      style("height", scaled(height * 0.0) + "px").
      style("opacity", PR_OP)
    d3.select("#" + id + "topleft").
      style("width", scaled(HANDLE_WIDTH + 2 * dtl) + "px").
      style("height", scaled(HANDLE_WIDTH + 2 * dtl) + "px").
      style("top", scaled(-dtl) + "px").
      style("left", scaled(-dtl) + "px").
      style("background-color", CORNER_COLOR).
      style("opacity", OPACITY).
      style("z-index", if (topleftdragunderway) "100" else "50").
      style("cursor", "nw-resize")
    d3.select("#" + id + "left").
      style("width", scaled(HANDLE_WIDTH + 2 * dd) + "px").
      style("height", scaled(height - 2 * HANDLE_WIDTH + 2 * dd) + "px").
      style("top", scaled(HANDLE_WIDTH - dd) + "px").
      style("left", scaled(-dd) + "px").
      style("background-color", BAR_COLOR).
      style("opacity", OPACITY).
      style("z-index", if (dragunderway) "110" else "50").
      style("cursor", "move")
    d3.select("#" + id + "bottomleft").
      style("width", scaled(HANDLE_WIDTH + 2 * dbl) + "px").
      style("height", scaled(HANDLE_WIDTH + 2 * dbl) + "px").
      style("top", scaled(height - HANDLE_WIDTH - dbl) + "px").
      style("left", scaled(-dbl) + "px").
      style("z-index", if (bottomleftdragunderway) "100" else "50").
      style("background-color", CORNER_COLOR).
      style("opacity", OPACITY).
      style("cursor", "sw-resize")
    d3.select("#" + id + "bottom").
      style("width", scaled(width - 2 * HANDLE_WIDTH + 2 * dd) + "px").
      style("height", scaled(HANDLE_WIDTH + 2 * dd) + "px").
      style("top", scaled(height - HANDLE_WIDTH - dd) + "px").
      style("left", scaled(HANDLE_WIDTH - dd) + "px").
      style("background-color", BAR_COLOR).
      style("opacity", OPACITY).
      style("cursor", "move")
    d3.select("#" + id + "bottomright").
      style("width", scaled(HANDLE_WIDTH + 2 * dbr) + "px").
      style("height", scaled(HANDLE_WIDTH + 2 * dbr) + "px").
      style("top", scaled(height - HANDLE_WIDTH - dbr) + "px").
      style("left", scaled(width - HANDLE_WIDTH - dbr) + "px").
      style("z-index", if (bottomrightdragunderway) "100" else "50").
      style("background-color", CORNER_COLOR).
      style("opacity", OPACITY).
      style("cursor", "se-resize")
    d3.select("#" + id + "right").
      style("width", scaled(HANDLE_WIDTH + 2 * dd) + "px").
      style("height", scaled(height - 2 * HANDLE_WIDTH + 2 * dd) + "px").
      style("top", scaled(HANDLE_WIDTH - dd) + "px").
      style("left", scaled(width - HANDLE_WIDTH - dd) + "px").
      style("background-color", BAR_COLOR).
      style("opacity", OPACITY).
      style("z-index", if (dragunderway) "110" else "50").
      style("cursor", "move")
    d3.select("#" + id + "top").
      style("width", scaled(width - 2 * HANDLE_WIDTH + 2 * dd) + "px").
      style("height", scaled(HANDLE_WIDTH + 2 * dd) + "px").
      style("top", scaled(-dd) + "px").
      style("left", scaled(HANDLE_WIDTH - dd) + "px").
      style("background-color", BAR_COLOR).
      style("opacity", OPACITY).
      style("z-index", if (dragunderway) "110" else "50").
      style("cursor", "move")
    d3.select("#" + id + "topright").
      style("width", scaled(HANDLE_WIDTH + 2 * dtr) + "px").
      style("height", scaled(HANDLE_WIDTH + 2 * dtr) + "px").
      style("left", scaled(width - HANDLE_WIDTH - dtr) + "px").
      style("top", scaled(-dtr) + "px").
      style("background-color", CORNER_COLOR).
      style("opacity", OPACITY).
      style("z-index", if (toprightdragunderway) "100" else "50").
      style("cursor", "ne-resize")
    d3.select("#" + id + "content").
      style("width", scaled(width - 2 * HANDLE_WIDTH) + "px").
      style("height", scaled(height - 2 * HANDLE_WIDTH) + "px").
      style("left", scaled(HANDLE_WIDTH) + "px").
      style("top", scaled(HANDLE_WIDTH) + "px").
      style("background-color", "#afafaf").
      style("z-index", "50").
      style("opacity", PR_OP).style("position", "absolute")
  }

  def build {
    widget.id = id + "content"

    me.html("")

    me.append("div").attr("id", id + "topleft").attr("draggable", "true").style("position", "absolute").style("opacity", PR_OP)
    me.append("div").attr("id", id + "left").attr("draggable", "true").style("position", "absolute").style("opacity", PR_OP)
    me.append("div").attr("id", id + "bottomleft").attr("draggable", "true").style("position", "absolute").style("opacity", PR_OP)
    me.append("div").attr("id", id + "top").attr("draggable", "true").style("position", "absolute").style("opacity", PR_OP)
    me.append("div").attr("id", id + "topright").attr("draggable", "true").style("position", "absolute").style("opacity", PR_OP)
    me.append("div").attr("id", id + "bottom").attr("draggable", "true").style("position", "absolute").style("opacity", PR_OP)
    me.append("div").attr("id", id + "bottomright").attr("draggable", "true").style("position", "absolute").style("opacity", PR_OP)
    me.append("div").attr("id", id + "right").attr("draggable", "true").style("position", "absolute").style("opacity", PR_OP)
    me.append("div").attr("id", widget.id).attr("draggable", "false").style("position", "absolute").style("opacity", PR_OP)

    topelement.addEventListener("dragstart", dragstarthandler)
    topelement.onmousedown = mousedownhandler
    topelement.onmouseup = mouseuphandler
    topelement.onmouseout = mouseuphandler
    topelement.onmousemove = mousemovehandler

    topleftelement.addEventListener("dragstart", topleftdragstarthandler)
    topleftelement.onmousedown = topleftmousedownhandler
    topleftelement.onmouseup = topleftmouseuphandler
    topleftelement.onmouseout = topleftmouseuphandler
    topleftelement.onmousemove = topleftmousemovehandler

    bottomleftelement.addEventListener("dragstart", bottomleftdragstarthandler)
    bottomleftelement.onmousedown = bottomleftmousedownhandler
    bottomleftelement.onmouseup = bottomleftmouseuphandler
    bottomleftelement.onmouseout = bottomleftmouseuphandler
    bottomleftelement.onmousemove = bottomleftmousemovehandler

    leftelement.addEventListener("dragstart", dragstarthandler)
    leftelement.onmousedown = mousedownhandler
    leftelement.onmouseup = mouseuphandler
    leftelement.onmouseout = mouseuphandler
    leftelement.onmousemove = mousemovehandler

    bottomelement.addEventListener("dragstart", dragstarthandler)
    bottomelement.onmousedown = mousedownhandler
    bottomelement.onmouseup = mouseuphandler
    bottomelement.onmouseout = mouseuphandler
    bottomelement.onmousemove = mousemovehandler

    toprightelement.addEventListener("dragstart", toprightdragstarthandler)
    toprightelement.onmousedown = toprightmousedownhandler
    toprightelement.onmouseup = toprightmouseuphandler
    toprightelement.onmouseout = toprightmouseuphandler
    toprightelement.onmousemove = toprightmousemovehandler

    bottomrightelement.addEventListener("dragstart", bottomrightdragstarthandler)
    bottomrightelement.onmousedown = bottomrightmousedownhandler
    bottomrightelement.onmouseup = bottomrightmouseuphandler
    bottomrightelement.onmouseout = bottomrightmouseuphandler
    bottomrightelement.onmousemove = bottomrightmousemovehandler

    rightelement.addEventListener("dragstart", dragstarthandler)
    rightelement.onmousedown = mousedownhandler
    rightelement.onmouseup = mouseuphandler
    rightelement.onmouseout = mouseuphandler
    rightelement.onmousemove = mousemovehandler
  }

}

class JSView(id: String) {
  import JSView._
  //var sa: ActorRef = null
  var sajs: JSActor = null

  def init {
    //sa = actor.system.actorOf(Props[ViewActor], "viewactor" + id)
    sajs = ViewActorJS()
  }

  var containers: Map[String, JSContainer] = Map[String, JSContainer]()

  def add(id: String, c: JSContainer) {
    containers += (id -> c)
    c.id = id
    c.parent = this
  }

  def MARGIN = 25.0

  def effectiveViewportWidth = Math.max(viewportWidth - rootleft - MARGIN, 100.0)
  def effectiveViewportHeight = Math.max(viewportHeight - roottop - MARGIN, 100.0)

  def horizontal = effectiveViewportWidth > effectiveViewportHeight

  var totalWidth = 0.0
  var totalHeight = 0.0

  var scalefactor = 0.0

  var firstrender: Boolean = true

  def upickleSer = JSViewSer(for ((k, v) <- containers.toMap) yield (k -> v.upickleSer))

  def upickleDeSer(s: JSViewSer) {
    for ((k, contser) <- s.containers) {
      if (!containers.contains(k)) {
        /*containers += (k -> new JSContainer(new JSWidget()))
        containers(k).upickleDeSer(contser)*/
      } else {
        containers(k).upickleDeSer(contser)
      }
    }
  }

  def upickleSerStr = write(upickleSer)

  def upickleDeSerStr(content: String) = {
    try {
      upickleDeSer(read[JSViewSer](content))
    } catch { case e: Throwable => { /*println("view deserialization failed on [" + content + "]")*/ } }
  }

  def serialize: String =
    {
      id + "#" + (for ((k, v) <- containers) yield (k + ":" + v.serialize)).mkString(";")
    }

  def deserialize(content: String) {
    if (content == "") return
    //containers = Map[String, Container]()
    val conts = content.split(";").toList
    for (cont <- conts) {
      val contparts = cont.split(":").toList
      val id = contparts(0)
      val content = contparts(1)
      val container = new JSContainer(new JSWidget())
      container.deserialize(content)
      containers += (id -> container)
    }
  }

  def render {
    var first = true
    var minx = 0.0; var maxx = 0.0; var miny = 0.0; var maxy = 0.0
    for ((k, v) <- containers) {
      if (first) {
        minx = v.left; maxx = v.right; miny = v.top; maxy = v.bottom
        first = false
      } else {
        if (v.left < minx) minx = v.left; if (v.right > maxx) maxx = v.right
        if (v.top < miny) miny = v.top; if (v.bottom > maxy) maxy = v.bottom
      }
    }
    totalWidth = Math.max(maxx - minx, 10.0)
    totalHeight = Math.max(maxy - miny, 10.0)
    scalefactor = if (horizontal) effectiveViewportHeight / totalHeight else effectiveViewportWidth / totalWidth
    for ((k, v) <- containers) {
      v.left -= minx
      v.top -= miny
      v.scalefactor = scalefactor
      v.widget.render
    }

    //if (!firstrender) sa ! StoreViewMessage(id, upickleSerStr) else firstrender = false
    if (!firstrender) sajs ! StoreViewMessage(id, upickleSerStr) else firstrender = false

  }

  def draw {
    for ((k, v) <- containers) {
      v.draw
      v.widget.draw
    }
  }

  def onresizehandler: js.Function1[dom.UIEvent, Unit] = (e: dom.UIEvent) => {
    renderAndDraw
  }

  var firstbuild = true

  def sortedcontainerkeys = containers.keys.toList.sortWith((a, b) => containers(a).top > containers(b).top)

  def build {
    root.style("position", "relative").style("opacity", "0.99")

    for (k <- sortedcontainerkeys) {
      val v = containers(k)
      if (firstbuild) root.append("div").attr("id", k).
        style("position", "absolute").style("opacity", "0.99")
      v.id = k
      v.parent = this
      v.build
    }
    firstbuild = false
    dom.window.onresize = onresizehandler
  }

  def tofront(which: String) {
    /*val elements = global.document.getElementById("root").childNodes.asInstanceOf[dom.NodeList]

    val clones = (for (i <- (0 to (elements.length - 1)) if (elements(i).isInstanceOf[dom.raw.HTMLDivElement])) yield {
      val clone = elements(i).cloneNode(true)
      val style = clone.attributes.getNamedItem("style").value
      val parts = style.split("top: ")
      val parts2 = parts(1).split("px")
      val px = parts2(0).toDouble
      Tuple2[dom.Node, Double](clone, px)
    }).toList

    val sorted = clones.sortWith((a, b) => a._2 > b._2)*/

    global.document.getElementById("root").innerHTML = ""

    /*for (e <- sorted) {
      global.document.getElementById("root").appendChild(e._1)
    }*/

    firstbuild = true
    build
    draw

    /*val element1 = dgebid("root").firstChild
    val element2 = dgebid(which)

    if (element1 == element2) { log("equal"); return }

    val clonedElement1 = element1.cloneNode(true);
    val clonedElement2 = element2.cloneNode(true);

    element2.parentNode.replaceChild(clonedElement1, element2);
    element1.parentNode.replaceChild(clonedElement2, element1);*/
  }

  def renderAndDraw {
    render
    draw
  }

  def buildRenderAndDraw {
    build
    renderAndDraw
  }

}