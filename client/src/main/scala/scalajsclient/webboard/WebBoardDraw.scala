package scalajsclient

import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.timers._
import scala.scalajs.js.Dynamic.global

import org.singlespaced.d3js.d3
import org.singlespaced.d3js.Ops._

import smartchess._
import ScalaJSUtils._
import shared._
import shared.SharedSettings._

object WebBoardDraw {
  import WebBoardGlobals._
  def drawaarrows(w: WebBoard) {
    implicit var scalefactor = w.scalefactor

    val tfen = w.g.report_trunc_fen
    val bpos = w.g.book.get(tfen)

    val aalgebs = bpos.arrowalgebs

    for (aalgeb <- aalgebs) {
      val av = w.algebtovect(aalgeb)

      val arrow = Arrow(av._1, av._2, color = AARROW_COLOR, constantwidth = scaled(SQUARE_SIZE / 5.0))
      w.board.append("div").
        style("position", "absolute").
        style("top", (arrow.svgorig.y) + "px").
        style("left", (arrow.svgorig.x) + "px").
        style("width", arrow.pg.size.x + "px").
        style("height", arrow.pg.size.y + "px").
        style("opacity", AARROW_OPACITY).
        style("z-index", "" + ARROW_ZINDEX).
        html(arrow.svg)
    }
  }
  def drawacontrols(w: WebBoard) {
    implicit var scalefactor = w.scalefactor

    for ((ac, i) <- ACONTROLS.view.zipWithIndex) {
      val svg = WebBoardPieces.CONTROL_SVGS(ac.function)

      w.d3acontrols.append("div").attr("id", w.acontrolsid(i)).
        style("position", "absolute").
        style("top", px(PLAYER_MARGIN)).
        style("left", px(PLAYER_MARGIN + i * (ACONTROL_WIDTH + PLAYER_MARGIN + 2.0 * ACONTROL_BORDER))).
        style("width", px(ACONTROL_WIDTH)).
        style("height", px(ACONTROL_HEIGHT)).
        style("background-color", ac.bcol).
        style("border-style", "solid").
        style("border-width", px(ACONTROL_BORDER)).
        style("border-color", "#afafaf").
        style("cursor", "pointer").
        html(svg)

      dgebid(w.acontrolsid(i)).onmousedown = w.acontrolmousedownhandler(ac)
    }
  }

  def drawplayerinfo(w: WebBoard, i: Int) {
    implicit var scalefactor = w.scalefactor

    val pc = PLAYER_COLORS(w.numplayers, i)

    if (w.player(i).handle == "") {
      if (w.canplay) w.d3playerinfo(i).
        append("div").attr("id", w.playbuttonid(i)).
        style("width", px(PLAYER_PLAY_WIDTH - PADDING_FACTOR * PLAYER_MARGIN)).
        style("height", px(PLAYER_HEIGHT)).
        style("padding-left", px(PADDING_FACTOR * PLAYER_MARGIN)).
        style("position", "absolute").
        style("top", px(PLAYER_MARGIN)).
        style("left", px(PLAYER_MARGIN)).
        style("background-color", pc._1).
        style("color", pc._2).
        style("font-size", px(PLAYER_FONT_SIZE)).
        style("cursor", "pointer").
        html(t("ccfg.play"))
      def displaythisai = (w.displayai && (i > 0))
      if (displaythisai) w.d3playerinfo(i).
        append("div").attr("id", w.aibuttonid(i)).
        style("width", px(PLAYER_AI_WIDTH - PADDING_FACTOR * PLAYER_MARGIN)).
        style("height", px(PLAYER_HEIGHT)).
        style("padding-left", px(PADDING_FACTOR * PLAYER_MARGIN)).
        style("position", "absolute").
        style("top", px(PLAYER_MARGIN)).
        style("left", px(PLAYER_MARGIN + PLAYER_HANDLE_WIDTH - PLAYER_AI_WIDTH)).
        style("background-color", pc._1).
        style("color", pc._2).
        style("font-size", px(PLAYER_FONT_SIZE)).
        style("cursor", "pointer").
        html(t("ccfg.playai"))

      if (w.canplay) dgebid(w.playbuttonid(i)).onmousedown = w.playmousedownhandler(i)
      if (displaythisai) dgebid(w.aibuttonid(i)).onmousedown = w.aimousedownhandler(i)
    } else {
      def showstand = (
        (!w.terminated)
        &&
        (
          (i == w.useri)
          ||
          (
            w.player(i).ai
            &&
            (!w.otherseated)
          )
        )
            &&
            (
              (!w.inprogress)
              ||
              (w.inprogress && w.isturn(i))
            )
      )

      val fh = w.player(i).formattedhandle(withrating = true, maxlength = if (w.IS_FOUR_PLAYER) 10 else -1)

      w.d3playerinfo(i).html("").
        append("div").
        style("width", px(PLAYER_HANDLE_WIDTH - PADDING_FACTOR * PLAYER_MARGIN)).
        style("height", px(PLAYER_HEIGHT)).
        style("padding-left", px(PADDING_FACTOR * PLAYER_MARGIN)).
        style("position", "absolute").
        style("top", px(PLAYER_MARGIN)).
        style("left", px(PLAYER_MARGIN)).
        style("background-color", pc._1).
        style("color", pc._2).
        style("font-size", px(PLAYER_FONT_SIZE)).
        html(fh)
      if (showstand) w.d3playerinfo(i).
        append("div").attr("id", w.standid(i)).
        style("width", px(CLOCK_WIDTH - PADDING_FACTOR * PLAYER_MARGIN)).
        style("height", px(PLAYER_HEIGHT)).
        style("padding-left", px(PADDING_FACTOR * PLAYER_MARGIN)).
        style("position", "absolute").
        style("top", px(PLAYER_MARGIN)).
        style("left", px(PLAYER_HANDLE_WIDTH + 2 * PLAYER_MARGIN)).
        style("background-color", pc._1).
        style("color", pc._2).
        style("font-size", px(PLAYER_FONT_SIZE)).
        style("cursor", "pointer").
        html(if (w.inprogress) t("ccfg.resign") else t("ccfg.stand"))
      w.d3playerclock(i).html("").
        style("background-color", if (w.isturn(i)) pc._2 else PLAYER_INFO_COLOR).
        append("div").attr("id", w.clockid(i)).
        style("width", px(CLOCK_WIDTH - 2 * PLAYER_MARGIN - PADDING_FACTOR * PLAYER_MARGIN)).
        style("height", px(PLAYER_HEIGHT)).
        style("padding-left", px(PADDING_FACTOR * PLAYER_MARGIN)).
        style("position", "absolute").
        style("top", px(PLAYER_MARGIN)).
        style("left", px(PLAYER_MARGIN)).
        style("background-color", pc._1).
        style("color", pc._2).
        style("font-size", px(PLAYER_FONT_SIZE)).
        html(w.player(i).formattime)

      if (showstand) dgebid(w.standid(i)).onmousedown = w.standmousedownhandler(i)
    }
  }

  def drawboardcontrols(w: WebBoard) {
    implicit var scalefactor = w.scalefactor
    w.d3boardcontrolstop.html("").
      append("div").
      style("position", "absolute").
      style("width", px(PLAYER_HANDLE_WIDTH - PADDING_FACTOR * PLAYER_MARGIN)).
      style("height", px(PLAYER_HEIGHT)).
      style("top", px(PLAYER_MARGIN)).
      style("left", px(PLAYER_MARGIN)).
      style("background-color", GAME_INFO_BCKG_COLOR).
      style("font-size", px(GAME_INFO_FONT_SIZE)).
      style("color", GAME_INFO_FONT_COLOR).
      style("padding-left", px(PADDING_FACTOR * PLAYER_MARGIN)).
      html(if (w.table.valid) w.table.gameinfo else "")

    if (!(w.inprogress && w.meseated)) {

      w.d3boardcontrolstop.
        append("div").attr("id", w.flipbuttonid).
        style("position", "absolute").
        style("width", px(BOARD_CONTROLS_BUTTON_WIDTH - PADDING_FACTOR * PLAYER_MARGIN)).
        style("height", px(BOARD_CONTROLS_BUTTON_HEIGHT)).
        style("top", px(PLAYER_MARGIN)).
        style("left", px(TOTAL_INFO_WIDTH - PLAYER_MARGIN - BOARD_CONTROLS_BUTTON_WIDTH)).
        style("background-color", BOARD_CONTROLS_BUTTON_BCKG_COLOR).
        style("color", BOARD_CONTROLS_BUTTON_FONT_COLOR).
        style("font-size", px(BOARD_CONTROLS_FONT_SIZE)).
        style("cursor", "pointer").
        style("padding-left", px(PADDING_FACTOR * PLAYER_MARGIN)).
        html(t("ccfg.flip"))

      dgebid(w.flipbuttonid).onmousedown = w.flipbuttonmousedownhandler
    }
  }

  def drawall(w: WebBoard) {
    implicit var scalefactor = w.scalefactor

    NUM_PLAYERS = w.b.numplayers

    w.clear

    FLICK_MARGIN = 10.0

    if (w.IS_FLICK) FLICK_MARGIN = 50.0

    w.render

    if (w.inprogress) w.flip = w.userflip

    w.boardcontainer.append("div").style("position", "absolute").attr("id", w.boardcontainerinnerid)

    val bctop = if (w.IS_ANALYSIS) ALL_ABOVE_BOARD_HEIGHT - PLAYER_INFO_HEIGHT else ALL_ABOVE_BOARD_HEIGHT

    w.boardcontainerinner.attr("draggable", "false").
      style("width", px(TOTAL_BOARD_WIDTH)).
      style("height", px(TOTAL_BOARD_HEIGHT)).
      style("top", px(bctop)).
      style("position", "absolute")

    w.boardcontainer.
      append("div").attr("id", "boardouterbackground").
      attr("draggable", "false").
      style("width", px(TOTAL_BOARD_WIDTH)).
      style("height", px(TOTAL_BOARD_HEIGHT)).
      style("top", px(bctop + FLICK_MARGIN)).
      style("left", px(FLICK_MARGIN)).
      style("position", "absolute").
      style("background", s"""url(/assets/images/backgrounds/wood.jpg)""")

    if (!w.IS_ANALYSIS) {
      w.boardcontainerinner.
        append("div").attr("id", w.boardcontrolstopid).
        style("position", "absolute").
        style("background-color", PLAYER_INFO_COLOR).
        style("top", px(-ALL_ABOVE_BOARD_HEIGHT)).
        style("width", px(TOTAL_INFO_WIDTH)).
        style("height", px(PLAYER_HEIGHT))
    }

    if (!w.IS_ANALYSIS) {
      for (i <- 0 to w.lastplayer) {
        val fi = (i + w.flip) % w.numplayers

        CALC_PLACINGS

        val pl = PLAYER_INFO_PLACINGS(w.numplayers, fi)
        w.boardcontainerinner.
          append("div").attr("id", w.playerinfoid(i)).
          style("position", "absolute").
          style("background-color", PLAYER_INFO_COLOR).
          style("top", px(pl._1)).
          style("left", px(pl._2)).
          style("width", px(PLAYER_INFO_WIDTH)).
          style("height", px(PLAYER_INFO_HEIGHT)).
          style("opacity", w.playeropacity(i))

        val pc = PLAYER_CLOCK_PLACINGS(w.numplayers, fi)
        w.boardcontainerinner.
          append("div").attr("id", w.playerclockid(i)).
          style("position", "absolute").
          style("background-color", PLAYER_INFO_COLOR).
          style("top", px(pc._1)).
          style("left", px(pc._2)).
          style("width", px(CLOCK_WIDTH)).
          style("height", px(PLAYER_INFO_HEIGHT)).
          style("z-index", "150").
          style("opacity", w.playeropacity(i))
      }
    }

    if (w.IS_ANALYSIS) {
      w.boardcontainerinner.
        append("div").attr("id", w.acontrolsid).
        style("position", "absolute").
        style("background-color", ACONTROLS_COLOR).
        style("top", px(TOTAL_INFO_HEIGHT)).
        style("left", px(0)).
        style("width", px(TOTAL_INFO_WIDTH)).
        style("height", px(2.0 * PLAYER_INFO_HEIGHT))

      drawacontrols(w)
    }

    if (!w.IS_ANALYSIS) {

      if (w.alreadyloaded) {

        for (i <- 0 to w.lastplayer) drawplayerinfo(w, i)

        drawboardcontrols(w)

      }

    }

    w.boardcontainerinner.attr("draggable", "false").
      append("div").attr("id", w.boardid)

    w.board.
      style("width", px(BOARD_WIDTH)).
      style("height", px(BOARD_HEIGHT)).
      style("position", "absolute").
      style("left", px(MARGIN + FLICK_MARGIN)).
      style("top", px(MARGIN + FLICK_MARGIN)).
      style("background", """url(/assets/images/backgrounds/wood.jpg)""")

    w.board.
      append("div").attr("id", "boardarrowdiv").
      attr("draggable", "false").
      style("position", "absolute")

    w.board.
      append("div").attr("id", "enginearrowdiv").
      attr("draggable", "false").
      style("position", "absolute")

    w.board.
      append("div").attr("id", "enginescorediv").
      attr("draggable", "false").
      style("position", "absolute")

    w.board.
      append("div").attr("id", "enginedepthdiv").
      attr("draggable", "false").
      style("position", "absolute")

    w.board.
      append("div").attr("id", "aarrowdiv").
      attr("draggable", "false").
      style("position", "absolute")

    w.board.
      append("div").attr("id", "promotiondiv").
      attr("draggable", "false").
      style("position", "absolute").
      style("width", BOARD_WIDTH).
      style("height", BOARD_HEIGHT).
      style("top", "0px").
      style("left", "0px").
      style("visibility", if (w.ispromoting) "visible" else "hidden").
      style("z-index", "200").
      style("opacity", "1.0")

    if (w.ispromoting) {
      val ppieces = if (!w.IS_FOUR_PLAYER) w.b.PROMOTION_PIECES.reverse else piece4.PROM_PIECE_CHARS
      var i = -1
      for (f <- ppieces :+ 'x') {
        i += 1
        val pid = "prompiece " + f
        var p: Any = null
        if (!w.IS_FOUR_PLAYER) {
          p = piece.fromFenChar(if (w.turn == 0) f.toUpper else f)
        } else {
          p = piece4("" + f, w.b.b4.turn4)
        }

        val bckg = if (w.IS_FOUR_PLAYER && ((w.turn == 0) || (w.turn == 1))) "blue" else "yellow"

        appendpiece(w, w.promotiondiv, pid, p, w.tofile, w.torank + i, bckg = if (f == 'x') "yellow" else bckg,
          cancel = if (f == 'x') "Cancel" else "")

        dgebid(pid).onmousedown = w.boardpromotionhandler
      }
    }

    val gameready = ((w.alreadyloaded) && (w.isUsersTurn) && (w.table.ready) && (!w.terminated))

    if (gameready || w.IS_ANALYSIS) {
      w.boardelement.onmousemove = w.boardmousemovehandler
      w.boardelement.onmousedown = w.boardmousedownhandler
      w.boardelement.onmouseup = w.boardmouseuphandler
    } else {
      w.boardelement.onmousemove = w.dummyhandler
      w.boardelement.onmousedown = w.dummyhandler
      w.boardelement.onmouseup = w.dummyhandler
    }

    for (i <- 0 to w.b.LAST_FILE) for (j <- 0 to w.b.LAST_RANK) {

      val pid = if (!w.IS_FOUR_PLAYER) w.pieceid(i, j) else "piece4" + square4(i, j).toalgeb
      val fpid = if (w.IS_FLICK) w.getfpid(pid) else ""

      var fp: FlickPiece = FlickPiece()

      if (w.b.flickpieces.pieces.contains(fpid)) fp = w.b.flickpieces.pieces(fpid)

      if (!w.IS_FOUR_PLAYER) {

        val p: piece.TPiece = if (!w.IS_FLICK) w.b.rep(w.trueindex(i, j)) else if (w.fallen(fp)) piece.NO_PIECE else fp.piece

        if (p != piece.NO_PIECE) {
          appendpiece(w, w.board, pid, p, i, j)

          dgebid(pid).addEventListener("dragstart", w.boarddragstarthandler)
        }

      } else {
        val p4 = w.b.b4.rep4.get(w.truesquare4(i, j))

        if (p4 != piece4.NO_PIECE) {
          val terminated = w.b.b4.IsTerminated(p4.color)

          appendpiece(w, w.board, pid, p4, i, j, makegrey = (terminated && w.inprogress))

          dgebid(pid).addEventListener("dragstart", w.boarddragstarthandler)
        }
      }

      val empty = if (w.IS_FOUR_PLAYER) square4(i, j).isempty else false

      if (!empty) w.board.append("div").attr("id", w.squareid(i, j)).attr("draggable", "false").
        style("width", px(SQUARE_SIZE)).
        style("height", px(SQUARE_SIZE)).
        style("background-color", List(DARK_SQUARE_COLOR, LIGHT_SQUARE_COLOR)((i + j + 1) % 2)).
        style("position", "absolute").
        style("left", px(i * SQUARE_SIZE)).
        style("top", px(j * SQUARE_SIZE)).
        style("z-index", "1").
        style("opacity", "" + SQUARE_OPACITY)

    }

    if (w.terminated) {
      w.boardcontainer.append("div").attr("id", w.coverid).
        style("position", "absolute").
        style("left", px(TERMINATED_MARGIN)).
        style("top", px(TERMINATED_MARGIN + ALL_ABOVE_BOARD_HEIGHT)).
        style("width", px(TOTAL_INFO_WIDTH - 2 * TERMINATED_MARGIN)).
        style("height", px(TOTAL_BOARD_HEIGHT - 2 * TERMINATED_MARGIN)).
        style("background-color", "#afafff").
        style("opacity", "0.5").
        style("z-index", "200").
        style("font-size", px(TERMINATED_FONT_SIZE)).
        style("font-weight", "bold").
        style("text-align", "center").
        html(s"""
          |<br>
          |${t("ccfg.terminated")}<br><br>
          |${w.table.result}<br><br>
          |${t(w.table.resultreason)}<br><br>
        """.stripMargin)
    }

    if (w.coveron) {
      w.boardcontainer.append("div").attr("id", w.coverid).
        style("position", "absolute").
        style("left", px(0.0)).
        style("top", px(0.0)).
        style("width", px(w.TOTAL_WIDTH)).
        style("height", px(w.TOTAL_HEIGHT)).
        style("background-color", "#afafaf").
        style("opacity", "0.5").
        style("z-index", "200")
      w.boardcontainer.append("div").
        style("position", "absolute").
        style("width", px(w.TOTAL_WIDTH * 0.8)).
        style("height", px(TOTAL_BOARD_HEIGHT * 0.8)).
        style("top", px(PLAYER_HEIGHT + PLAYER_INFO_HEIGHT + TOTAL_BOARD_HEIGHT * 0.1)).
        style("left", px(w.TOTAL_WIDTH * 0.1)).
        style("opacity", "0.4").
        style("z-index", "201").
        html(svgspinner)
    }

    if (w.table.algebline != "") {
      val algebs = w.table.algebline.split(" ")
      val algeb = algebs.reverse.head
      val av = w.algebtovect(algeb)
      val arrow = Arrow(av._1, av._2, constantwidth = scaled(SQUARE_SIZE / 5.0))
      w.boardarrowdiv.
        style("top", (arrow.svgorig.y) + "px").
        style("left", (arrow.svgorig.x) + "px").
        style("width", arrow.pg.size.x + "px").
        style("height", arrow.pg.size.y + "px").
        style("opacity", "0.25").
        style("z-index", "" + ARROW_ZINDEX).
        html(arrow.svg)
    }

    if ((w.IS_FOUR_PLAYER) && (w.b.b4.lastalgeb != "-")) {
      val av = w.algeb4tovect(w.b.b4.lastalgeb)
      val arrow = Arrow(av._1, av._2, constantwidth = scaled(SQUARE_SIZE / 5.0))
      w.boardarrowdiv.
        style("top", (arrow.svgorig.y) + "px").
        style("left", (arrow.svgorig.x) + "px").
        style("width", arrow.pg.size.x + "px").
        style("height", arrow.pg.size.y + "px").
        style("opacity", "0.25").
        style("z-index", "" + ARROW_ZINDEX).
        html(arrow.svg)
    }

    if (w.IS_ANALYSIS) {
      drawaarrows(w)
    }

  }

  def appendpiece(
    w: WebBoard,
    selection: org.singlespaced.d3js.Selection[org.scalajs.dom.EventTarget],
    pid: String,
    pany: Any,
    file: Int,
    rank: Int,
    bckg: String = "none",
    cancel: String = "",
    makegrey: Boolean = false
  ) {
    implicit var scalefactor = w.scalefactor
    if (cancel != "") {
      val a = selection.append("div").attr("id", pid).attr("draggable", "true").
        style("height", px(PIECE_SIZE)).
        style("background-size", "contain").
        style("position", "absolute").
        style("left", px(w.truepiececoord(file))).
        style("top", px(w.truepiececoord(rank))).
        style("z-index", "50").
        style("padding", px(SQUARE_SIZE * 0.2)).
        style("font-size", px(SQUARE_SIZE * 0.8)).
        style("cursor", "pointer").
        html(cancel)

      if (bckg != "none") a.style("background-color", bckg)
    } else if (pany.isInstanceOf[piece4]) {
      val p4 = pany.asInstanceOf[piece4]
      val kind = if (p4.color >= 2) p4.kind else p4.kind.toUpperCase

      var svg = WebBoardPieces.black(p4.kind)

      val pc = PLAYER_COLORS(4, p4.color)
      val selcol = if (!makegrey) pc._1 else "#7f7f7f"
      svg = svg.replaceAll("fill=.........", s"""fill="$selcol"""")

      val a = selection.append("div").attr("id", pid).attr("draggable", "true").
        style("width", px(PIECE_SIZE)).
        style("height", px(PIECE_SIZE)).
        //style("background-size", "contain").
        style("position", "absolute").
        style("left", px(w.truepiececoord(file))).
        style("top", px(w.truepiececoord(rank))).
        style("z-index", "50").
        //classed(PIECE_STYLE + "piece" + kind, true).
        style("transform", s"rotate(${COLOR4_PROPERTIES((p4.color + w.flip) % board4.NUM_PLAYERS).rot}deg)").
        html(svg)

      if (bckg != "none") a.style("background-color", bckg)

      /*a.
        append("div").attr("id", pid + "cover4").
        style("width", px(SQUARE_SIZE - 2 * PADDING)).
        style("height", px(SQUARE_SIZE - 2 * PADDING)).
        style("position", "absolute").
        style("left", "0px").
        style("top", "0px").
        style("background-color", COLOR4_PROPERTIES(p4.color).color).
        style("opacity", "0.2")*/
    } else {
      val p = pany.asInstanceOf[piece.TPiece]

      val fpid = if (w.IS_FLICK) w.getfpid(pid) else ""

      var fp: FlickPiece = FlickPiece()

      if (w.b.flickpieces.pieces.contains(fpid)) fp = w.b.flickpieces.pieces(fpid)

      val a = selection.append("div").attr("id", pid).attr("draggable", "true").
        style("width", px(PIECE_SIZE)).
        style("height", px(PIECE_SIZE)).
        style("background-size", "contain").
        style("position", "absolute").
        style("left", if (!w.IS_FLICK) px(w.truepiececoord(file)) else px(w.flickscreencoord(fp.x))).
        style("top", if (!w.IS_FLICK) px(w.truepiececoord(rank)) else px(w.flickscreencoord(fp.y))).
        style("z-index", "50").
        style("opacity", "" + PIECE_OPACITY(piece.colorOf(p))).
        classed(PIECE_STYLE + "piece" + piece.toFenChar(p), true)

      if (bckg != "none") a.style("background-color", bckg)
    }
  }
}