package scalajsclient

import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.timers._
import scala.scalajs.js.Dynamic.global

import org.singlespaced.d3js.d3
import org.singlespaced.d3js.Ops._

import smartchess._

import ScalaJSUtils._
import scalajsclient._
import scalajsclient.jswidgets._
import scalajsclient.jswidgets.JSButton._

import upickle.default._
import shared._

import scala.concurrent.duration._

import shared.SharedSettings._

object handlers {
  import globals._
  val createtablehandler: ButtonHandler = (b: JSButton) => {
    val p = b.parent.parent.containers
    val variant = p("variant").widget.asInstanceOf[JSCombo].current
    val timecontrol = p("timecontrol").widget.asInstanceOf[JSCombo].current
    ts.sajs ! CreateTableMsg(variant, timecontrol, b)
  }
}

object globals {
  var wb: WebBoard = null
  var ts: JSTables = null
  var sajs: JSActor = null
  var tabs: JSTabpane = null
  var analysisvariantcombo: JSCombo = null
}

case class MainActorJS() extends SocketActorJS {
  def receive = {
    case SocketOpenedMsg => //sendMsg(HelloMessage("main"))
    case HelloMessage(content) => //println("got hello: " + content)
    case _ => println("This was unexpected.")
  }
}

object ScalaJSExample extends js.JSApp {
  import handlers._
  import globals._

  def main(): Unit = {

    println("JS app started")

    SocketActorJS.setupfromurl(dom.window.location.href)

    clearInfo

    val viewid = root.attr("viewid")

    val ccfg = s("chessconfig").html().replaceAll("^\\s*|\\s*$", "")

    val chessconfig = read[ChessConfig](ccfg)

    implccfg = chessconfig

    if ((viewid == "chess") && (chessconfig.kind == "analysis")) {
      val v = new JSView("analysis")

      wb = new WebBoard("board", java.util.UUID.randomUUID().toString, purpose = "analysis", setchessconfig = chessconfig)

      def SIDE_OFFSET = wb.TOTAL_WIDTH + 25.0

      analysisvariantcombo = new JSCombo(
        "variant",
        SharedSettings.ANALYZED_VARIANTS,
        Some(SharedSettings.DEFAULT_ANALYZED_VARIANT),
        selectedcallback = wb.analysisvariantselectedcallback
      )

      tabs = new JSTabpane("tabs", tabs = List(
        JSTab("book", t("ccfg.book")),
        JSTab("pgn", "PGN"),
        JSTab("import", "Import"),
        JSTab("pres", t("ccfg.presentation")),
        JSTab("notes", t("ccfg.notes")),
        JSTab("essay", t("ccfg.essay")),
        JSTab("log", "Log")
      ))

      v.add("board", new JSContainer(wb, 0.0, 0.0, snap = true))
      v.add("variant", new JSContainer(analysisvariantcombo, 0.0, SIDE_OFFSET, snap = true))
      v.add("tabs", new JSContainer(tabs, 50.0, SIDE_OFFSET, snap = true))

      v.upickleDeSerStr(root.attr("viewserialized"))
      v.buildRenderAndDraw

      movetable.init("Standard")
      v.init
      sajs = MainActorJS()
      wb.init

      println("analysis initialized")
    }

    if ((viewid == "chess") && (chessconfig.kind == "play")) {
      val v = new JSView("chess")

      wb = new WebBoard("board", java.util.UUID.randomUUID().toString) // unique id
      ts = new JSTables("tables")

      def SIDE_OFFSET = wb.TOTAL_WIDTH + 25.0

      val timecontrolcombo = new JSCombo("timecontrol", SharedSettings.SUPPORTED_TIME_CONTROLS, Some(DEFAULT_TIME_CONTROL))
      val variantcombo = new JSCombo("variant", SharedSettings.SUPPORTED_VARIANTS, Some(DEFAULT_VARIANT))

      v.add("board", new JSContainer(wb, 0.0, 0.0, snap = true))
      v.add("timecontrol", new JSContainer(timecontrolcombo, 35.0, SIDE_OFFSET, snap = true))
      v.add("variant", new JSContainer(variantcombo, 0.0, SIDE_OFFSET, snap = true))
      v.add("creategame", new JSContainer(new JSButton("creategame", t("ccfg.createtable"), t("ccfg.creatingtable"), createtablehandler), 70.0, SIDE_OFFSET, snap = true))
      v.add("tables", new JSContainer(ts, 120.0, SIDE_OFFSET, snap = true))

      v.upickleDeSerStr(root.attr("viewserialized"))
      v.buildRenderAndDraw

      coverInfo(t("ccfg.connecting"))

      movetable.init("Standard")
      v.init
      sajs = MainActorJS()
      ts.init
      wb.init

      println("chess initialized")

    }

  }
}

