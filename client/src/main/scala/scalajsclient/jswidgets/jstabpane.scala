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

object JSTabpane {
}

case class JSTab(
    id: String,
    caption: String
) {
}

case class Tabcontent(
  content: String = "",
  buttonids: List[String] = List[String](),
  buttoncallback: (String) => Unit = null,
  selectids: List[String] = List[String](),
  selectcallback: (String, String) => Unit = null,
  vscroll: Double = 0.0
)

class JSTabpane(
    setid: String,
    width: Double = 500.0,
    height: Double = 430.0,
    tabs: List[JSTab] = List(JSTab("main", "Main"))
) extends JSSubWidget {
  id = setid
  override def TOTAL_WIDTH = width
  override def TOTAL_HEIGHT = height
  fixedratio = true

  var selectedtabid = tabs(0).id

  var contents = Map[String, Tabcontent]()

  def setcontent(tid: String, content: Tabcontent = Tabcontent()) = {
    contents += (tid -> content)
  }

  for (tab <- tabs) setcontent(tab.id)

  def tabid(tid: String) = id + "tab" + tid
  def d3tab(tid: String) = s("#" + tabid(tid))

  def tabdivid(tid: String) = id + "tabdiv" + tid
  def d3tabdiv(tid: String) = s("#" + tabdivid(tid))

  override def render {
    renderhelper
  }

  def selecttab(tid: String) {
    selectedtabid = tid
    draw
  }

  def tabmousedownhandler(tid: String): MouseHandler = (e: dom.MouseEvent) => {
    selecttab(tid)
  }

  def contentmousedownhandler(tid: String, bid: String): MouseHandler = (e: dom.MouseEvent) => {
    contents(tid).buttoncallback(bid)
  }

  def selectchangedhandler(tid: String, sid: String): MouseHandler = (e: dom.MouseEvent) => {
    contents(tid).selectcallback(sid, dgebid(sid).value.toString())
  }

  override def draw {
    initdraw
    parentdown

    val d3tabs = d3content.append("div").
      attr("class", "tab")

    for (tab <- tabs) d3tabs.append("button").
      attr("id", tabid(tab.id)).
      attr("class", "tablinks" + (if (tab.id == selectedtabid) " active" else "")).
      html(tab.caption)

    for (tab <- tabs) d3content.append("div").
      attr("id", tabdivid(tab.id)).
      attr("class", "tabcontent").
      style("display", if (tab.id == selectedtabid) "block" else "none").
      style("width", px(scaled(width) - 27)).
      style("height", px(scaled(height) - 59)).
      html(contents(tab.id).content)

    for (tab <- tabs if (contents(tab.id).vscroll != 0.0)) {
      val elem = dgebid(tabdivid(tab.id))
      val scrollheight = elem.scrollHeight.toString().toDouble
      var calcheight = scrollheight * contents(tab.id).vscroll
      if (calcheight > scrollheight) calcheight = scrollheight
      if (calcheight < 0.0) calcheight = 0.0
      elem.scrollTo(0.0, calcheight)
    }

    for (tab <- tabs) {
      dgebid(tabid(tab.id)).onmousedown = tabmousedownhandler(tab.id)
      for (bid <- contents(tab.id).buttonids) {
        dgebid(bid).onmousedown = contentmousedownhandler(tab.id, bid)
      }
      for (sid <- contents(tab.id).selectids) {
        dgebid(sid).addEventListener("change", selectchangedhandler(tab.id, sid))
      }
    }
  }
}