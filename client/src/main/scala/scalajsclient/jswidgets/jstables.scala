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
import shared._

import upickle.default._

import scala.concurrent.duration._

import shared.SharedSettings._

object JSTables {
  val REFRESH_TABLES_TIMEOUT = Some(10000)
}

case class CreateTableMsg(variant: String, timecontrol: String, button: JSButton = null)

case class TablesActorJS(myts: JSTables) extends SocketActorJS {
  import JSTables._
  var button: JSButton = null
  def requesttables = sendMsg(SendTablesMessage("all"))
  var first = true
  def receive = {
    ////////////////////////////////////////////////////////////////
    case SocketOpenedMsg => requesttables
    case TableCreationResultMessage(result) => {
      if (!result) global.alert("Table could not be created!")
      if (button != null) {
        button.pushed = false
        button.draw
      }
    }
    case SendTablesResultMessage(tables) => {
      myts.tables = tables
      myts.draw
      if (first) {
        first = false
        clearInfo
      }
      if (!REFRESH_TABLES_TIMEOUT.isEmpty) setTimeout(REFRESH_TABLES_TIMEOUT.get) { requesttables }
    }
    ////////////////////////////////////////////////////////////////
    case CreateTableMsg(variant, timecontrol, b) => {
      sendMsg(CreateTableMessage(variant, timecontrol, globals.wb.webboardid))
      button = b
      setTimeout(1000) { requesttables }
    }
    case x: DeleteTableMessage => {
      sendMsg(x)
      setTimeout(1000) { requesttables }
    }
    case x: SendTablesMessage => requesttables
    ////////////////////////////////////////////////////////////////
    case _ => println("This was unexpected.")
  }
}

class JSTables(
    setid: String
) extends JSSubWidget {

  id = setid

  override def TOTAL_WIDTH = 500.0
  override def TOTAL_HEIGHT = 360.0

  fixedratio = false

  var tables = Map[String, Table]()
  def tablekeyssorted = tables.keys.toList.sortWith(sorttablekeysfunc)

  var sajs: JSActor = null

  def init {
    sajs = TablesActorJS(this)
  }

  val delmousedownhandler: js.ThisFunction1[dom.raw.HTMLElement, dom.MouseEvent, Unit] = (de: dom.raw.HTMLElement, e: dom.MouseEvent) => {
    val parts = de.id.split("__")
    val k = parts(1)
    sajs ! DeleteTableMessage(k)
  }

  val loadmousedownhandler: js.ThisFunction1[dom.raw.HTMLElement, dom.MouseEvent, Unit] = (de: dom.raw.HTMLElement, e: dom.MouseEvent) => {
    val parts = de.id.split("__")
    val k = parts(1)
    globals.wb.sajs ! RegisterWebBoardMessage(globals.wb.webboardid, k, user)
  }

  val refreshmousedownhandler: js.ThisFunction1[dom.raw.HTMLElement, dom.MouseEvent, Unit] = (de: dom.raw.HTMLElement, e: dom.MouseEvent) => {
    sajs ! SendTablesMessage("all")
  }

  override def render {
  }

  def getcid(action: String, k: String) = id + action + "__" + k

  var firstdraw = true

  def status(tbl: Table): String = {
    if (tbl.terminated) return s"${t("ccfg.terminated")} , ${t("ccfg.result")} : " + tbl.result + " , " + t(tbl.resultreason)
    if (tbl.inprogress) return s"${t("ccfg.inprogress")}"
    s"${t("ccfg.open")}"
  }

  override def draw {
    var oldScrollTop: String = "0"
    var oldScrollLeft: String = "0"
    if (!firstdraw) try {
      oldScrollTop = dgebid(contentid).scrollTop.toString
      oldScrollLeft = dgebid(contentid).scrollLeft.toString
    } catch { case e: Throwable => { /*println("problem establishing scroll top")*/ } }
    initdraw
    d3content.style("overflow", "scroll")
    if ((parent != null) && (parent.isInstanceOf[JSContainer])) {
      d3content.
        style("width", parent.effwidth + "px").
        style("height", parent.effheight + "px")
    }
    val td = """td class="devtd""""

    val tablescontent = (for (k <- tablekeyssorted) yield {
      val v = tables(k)
      val IS_4PLAYER = (v.variant == "Four Player")
      val cid = getcid("delete", k)
      val lid = getcid("load", k)
      s"""
        |<tr>
        |<$td id="$lid"><div class="loadtable">${t("ccfg.load")}</div></td>        
        |<$td><div class="tableplayer">${v.players(0).formattedhandle(withrating = true)}</div></td>
        |<$td><div class="tableplayer">${v.players(if (IS_4PLAYER) 2 else 1).formattedhandle(withrating = true)}</div></td>
        |<$td><div class="tableplayer">${if (IS_4PLAYER) v.players(1).formattedhandle(withrating = true) else ""}</div></td>
        |<$td><div class="tableplayer">${if (IS_4PLAYER) v.players(3).formattedhandle(withrating = true) else ""}</div></td>
        |<$td><div class="tablevariant">${v.variant}</div></td>
        |<$td><div class="tabletimecontrol">${v.timecontrol}</div></td>
        |${if (admin) s"""<$td id="$cid">Del</td>""" else ""}
        |<td></td>
        |</tr>
        |<tr>
        |<$td colspan="7">
        |<div class="tablestatus ${v.statusclass}">        
        |${t("ccfg.created")}: ${v.createdF}<br>
        |${t("ccfg.status")}: ${status(v)}
        |</div>
        |</td>
        |</tr>
      """.stripMargin
    }).mkString("\n")

    val rid = getcid("refresh", "")

    val content = s"""            
      |<div id="$rid" class="refreshtables">${t("ccfg.refresh")}</div>
      |<table>
      |<tr class="tablesheader">
      |<$td></td>
      |<$td>${t("ccfg.white")}</td>
      |<$td>${t("ccfg.black")}</td>
      |<$td>${t("ccfg.yellow")}</td>
      |<$td>${t("ccfg.red")}</td>
      |<$td>${t("ccfg.variant")}</td>
      |<$td>${t("ccfg.timecontrol")}</td>
      |</tr>      
      |$tablescontent
      |</table>
      |</div>
    """.stripMargin

    d3content.html(content)
    d3content.select("#" + rid).style("cursor", "pointer")
    dgebid(rid).onmousedown = refreshmousedownhandler

    for ((k, v) <- tables) {
      val cid = getcid("delete", k)
      val lid = getcid("load", k)
      d3content.select("#" + cid).style("cursor", "pointer")
      d3content.select("#" + lid).style("cursor", "pointer")
      if (admin) dgebid(cid).onmousedown = delmousedownhandler
      dgebid(lid).onmousedown = loadmousedownhandler
    }

    dgebid(contentid).scrollTop = oldScrollTop
    dgebid(contentid).scrollLeft = oldScrollLeft

    firstdraw = false
  }

  def sorttablekeysfunc(keya: String, keyb: String): Boolean = {
    val a = tables(keya)
    val b = tables(keyb)
    SharedLogic.sorttablekeyslogic(a, b)
  }

  def hastable(k: String): Boolean = tables.contains(k)
}