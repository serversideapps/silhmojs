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

import scalajsclient.jswidgets._

case class ThinkingOutput(
    var bestmovealgeb: String = "",
    var scorecp: Boolean = true,
    var scoremate: Boolean = false,
    var score: Int = 0,
    var depth: Int = 0
) {
  def update(buffer: String) {
    if (buffer.contains(" pv ")) {
      val parts = buffer.split(" pv ")
      if (parts.length > 1) {
        val algebparts = parts(1).split(" ")
        bestmovealgeb = algebparts(0)
      }
    }
    if (buffer.contains(" score cp ")) {
      val parts = buffer.split(" score cp ")
      if (parts.length > 1) {
        val scorecpparts = parts(1).split(" ")
        score = scorecpparts(0).toInt
        scorecp = true
        scoremate = false
      }
    }
    if (buffer.contains(" score mate ")) {
      val parts = buffer.split(" score mate ")
      if (parts.length > 1) {
        val scoremateparts = parts(1).split(" ")
        score = scoremateparts(0).toInt
        scorecp = false
        scoremate = true
      }
    }
    if (buffer.contains(" depth ")) {
      val parts = buffer.split(" depth ")
      if (parts.length > 1) {
        val depthparts = parts(1).split(" ")
        depth = depthparts(0).toInt
      }
    }
  }
}

case class EngineSocketActorJS(wb: WebBoard, openedcallback: () => Unit) extends AbstractEngineSocketActorJS {
  def receive = {
    case SocketOpenedMsg => openedcallback()
    case x: EngineMessage => wb.enginemessagereceived(x)
    case a: Any => println("This was unexpected. + " + a)
  }
}

case class WebBoardActorJS(wb: WebBoard) extends SocketActorJS {
  def requestdefaulttable = sendMsg(SendTableMessage("default"))
  def receive = {
    case SocketOpenedMsg => requestdefaulttable
    case x: SitPlayerMessage => sendMsg(x)
    case SitPlayerResultMessage(success, receivedwebboardid) => wb.sitplayerresult(success, receivedwebboardid)
    case x: SendTableMessage => sendMsg(x)
    case SendTableResultMessage(k, table, receivedwebboardid) => wb.setfromtable(k, table, receivedwebboardid)
    case x: SendMoveMessage => sendMsg(x)
    case x: RegisterWebBoardMessage => sendMsg(x)
    case x: StorePresentationMessage => sendMsg(x)
    case StorePresentationResultMessage(success: Boolean) => wb.storepresdone(this, success)
    case a: Any => println("This was unexpected. + " + a)
  }
}

case class PresentationUploadActorJS(wb: WebBoard) extends SocketActorJS {
  def receive = {
    case SocketOpenedMsg => wb.uploadopened(this)
    case x: StorePresentationMessage => sendMsg(x)
    case StorePresentationResultMessage(success: Boolean) => wb.storepresdone(this, success)
    case a: Any => println("This was unexpected. + " + a)
  }
}

class WebBoard(
    setid: String,
    setwebboardid: String,
    purpose: String = "main",
    setchessconfig: ChessConfig = ChessConfig()
) extends JSWidget {
  import WebBoardGlobals._
  import WebBoardDraw._

  ///////////////////////////////////////////////////////////////////////////

  var sajs: JSActor = null

  var alreadyloaded = false

  def init {
    sajs = WebBoardActorJS(this)

    if (IS_MAIN) {
      setTimeout(3000) { loadsomething }
    }

    if (IS_ANALYSIS) loadsomething

  }

  ///////////////////////////////////////////////////////////////////////////

  id = setid
  var webboardid = setwebboardid

  implicit var scalefactor = MyFactor()

  var chessconfig = setchessconfig

  var b = new board
  b.reset

  var g = new game
  g.reset

  fixedratio = true

  var table: Table = Table("0", smartchess.board.DEFAULT_VARIANT, "3 + 2", valid = false)

  var flip: Int = 0

  var coveron = false
  var tableid: String = "default"

  override def draw = drawall(this)
  override def TOTAL_HEIGHT = TOTAL_BOARD_HEIGHT + 2 * PLAYER_INFO_HEIGHT + PLAYER_HEIGHT + 2 * FLICK_MARGIN
  override def TOTAL_WIDTH = TOTAL_BOARD_WIDTH + 2 * FLICK_MARGIN

  override def hide {
    boardcontainer.style("visibility", "hidden")
  }

  override def show {
    boardcontainer.style("visibility", "visible")
  }

  override def render {
    NUM_PLAYERS = numplayers
    val peh = parent.effheight
    val pew = parent.effwidth
    scalefactor = MyFactor(peh / TOTAL_HEIGHT)
    if (effective(TOTAL_WIDTH) > pew) scalefactor = MyFactor(pew / TOTAL_WIDTH)
  }

  setTimeout(1000) {

    dectime

  }

  setTimeout(10000) {

    checkidle

  }

  ///////////////////////////////////////////////////////////////////////////

  var enginerunning = false
  var thinkingoutput = ThinkingOutput()
  var availableengines = List[String]()
  var enginesocket: EngineSocketActorJS = null
  var logger = Logger()
  var aarrowon = false
  def presmine = (presentation.owner == user)
  def preseditable = presmine && (presentation.canedit != "no")
  def protanddomain = getprotanddomain(dom.window.location.href)
  def BASE_URL = protanddomain._1 + "://" + protanddomain._2
  def prurl = BASE_URL + "/analysis/" + presentation.id
  def inprogress = table.inprogress
  def useri = table.getuseri(user)
  def meseated = (useri >= 0)
  def anyseated = table.anyseated
  def otherseated = (anyseated && (!meseated))
  def hashuman = table.hashuman
  def canplay = (!meseated)
  def displayai = ((!otherseated) && table.players(0).validhuman)
  def isUsersTurn = if (!IS_FLICK) useri == turn else if (useri == 0) b.flickturn == "white" else b.flickturn == "black"
  def anon = (user == "")
  def terminated = table.terminated
  def boardid = id + "board"
  def playerinfoid(i: Int) = id + "playerinfo" + i
  def playerclockid(i: Int) = id + "playerclock" + i
  def d3playerinfo(i: Int) = d3.select("#" + playerinfoid(i))
  def d3playerclock(i: Int) = d3.select("#" + playerclockid(i))
  def acontrolsid = id + "acontrols"
  def acontrolsid(i: Int) = id + "acontrols" + i
  def d3acontrols = d3.select("#" + acontrolsid)
  def boardcontainerinnerid = id + "boardcontainerinner"
  def boardcontainer = s(id)
  def boardcontainerinner = s(boardcontainerinnerid)
  def board = s(boardid)
  def algebfr(file: Int, rank: Int): String = square.toAlgeb(square.fromFileRank(file, rank))
  def coordindex(coord: Double)(implicit f: MyFactor = MyFactor()): Int = (coord / f.factor).toInt / SQUARE_SIZE.toInt
  def truecoordindex(coord: Double) = if (flip == 0) coordindex(coord) else b.LAST_FILE - coordindex(coord)
  def flickscreencoord(coord: Double) = trueflickcoord(coord) * SQUARE_SIZE + PADDING
  def flickcoordindex(coord: Double)(implicit f: MyFactor = MyFactor()): Double = (coord / f.factor) / SQUARE_SIZE
  def pieceid(file: Int, rank: Int) = id + "piece_" + (if (!IS_FLICK) algebfr(file, rank) else trueindex(file, rank))
  def flickpieceid(flickid: String) = id + "piece_" + flickid
  def getfpid(pid: String): String = pid.split("_")(1)
  def squareid(file: Int, rank: Int) = id + "square" + algebfr(file, rank)
  def index(file: Int, rank: Int) = rank * b.NUM_FILES + file
  def truex(clientX: Double) = clientX - boardleft + offsetx
  def truey(clientY: Double) = clientY - boardtop + offsety
  def atruex(clientX: Double) = clientX - boardleft
  def atruey(clientY: Double) = clientY - boardtop
  def truesquare4(file: Int, rank: Int) = square4(file, rank).rot(-flip)
  def truepiececoord(coord: Double) = coord + PADDING
  def truepiececoord(coord: Int): Double = coord * SQUARE_SIZE + PADDING
  def trueflickcoord(coord: Double) = if (flip == 1) b.LAST_FILE.toDouble - coord else coord
  def boardelement = dgebid(boardid)
  def boardleft = gbcrleftd(boardelement)
  def boardtop = gbcrtopd(boardelement)
  var offsetx: Double = 0.0
  var offsety: Double = 0.0
  var origx: Double = 0.0
  var origy: Double = 0.0
  def finalx = getpx(s(draggedid).style("left"))
  def finaly = getpx(s(draggedid).style("top"))
  var dragunderway: Boolean = false
  var draggedid: String = ""
  var draggedfpid: String = ""
  var draggedalgeb: String = ""
  var draggedalgeb4: String = ""
  def isturn(i: Int) = (i == turn)
  def playbuttonid(i: Int) = id + "playbutton" + i
  def aibuttonid(i: Int) = id + "aibutton" + i
  def standid(i: Int) = id + "stand" + i
  def numplayers = b.numplayers
  def lastplayer = numplayers - 1
  def variant = table.variant
  def flipbuttonid = id + "flipbutton"
  def boardcontrolstopid = id + "boardcontrolstop"
  def coverid = id + "boardcover"
  def resultcoverid = id + "resultboardcover"
  def player(i: Int) = table.players(i)
  def clockid(i: Int) = id + "clock" + i
  def d3boardcontrolstop = d3.select("#" + boardcontrolstopid)
  def d3cover = d3.select("#" + coverid)
  def effective(coord: Double) = scalefactor.factor * coord
  def boardarrowdiv = s("boardarrowdiv")
  def enginearrowdiv = s("enginearrowdiv")
  def enginedepthdiv = s("enginedepthdiv")
  def enginescorediv = s("enginescorediv")
  def aarrowdiv = s("aarrowdiv")
  def promotiondiv = s("promotiondiv")
  def clearboardarrow { boardarrowdiv.html("") }
  def clearaarrow { aarrowdiv.html("") }
  def IS_FLICK = (variant == "Flick")
  def IS_FOUR_PLAYER = (variant == "Four Player")
  def IS_MAIN = (purpose == "main")
  def IS_ANALYSIS = (purpose == "analysis")
  var ispromoting = false
  var promotionalgeb = ""
  var promotionalgeb4 = ""
  var fromfile = 0
  var fromrank = 0
  var tofile = 0
  var torank = 0
  var afromfile = 0
  var afromrank = 0
  var atofile = 0
  var atorank = 0
  def userflip = if (!IS_FOUR_PLAYER) if (useri >= 0) useri else 0 else if (useri >= 0) List(0, 3, 2, 1)(useri) else 0
  def playeropacity(i: Int) = if (player(i).resigned) "0.3" else "1.0"
  var idlecount = 0
  def presentation = chessconfig.presentation
  def presgame = chessconfig.presgame

  ///////////////////////////////////////////////////////////////////////////

  def truefile(file: Int): Int = {
    if (numplayers == 2) {
      if (flip == 0) return file
      return b.LAST_FILE - file
    }
    file
  }

  def truerank(rank: Int): Int = {
    if (numplayers == 2) {
      if (flip == 0) return rank
      return b.LAST_RANK - rank
    }
    rank
  }

  def algebtovect(algeb: String): Tuple2[Vect, Vect] = {
    val m = move(fromalgeb = algeb)
    val filefrom = square.fileOf(m.from)
    val rankfrom = square.rankOf(m.from)
    val truefilefrom = truefile(filefrom)
    val truerankfrom = truerank(rankfrom)
    val from = Vect(truefilefrom, truerankfrom) * scaled(SQUARE_SIZE)
    val fileto = square.fileOf(m.to)
    val rankto = square.rankOf(m.to)
    val truefileto = truefile(fileto)
    val truerankto = truerank(rankto)
    val to = Vect(truefileto, truerankto) * scaled(SQUARE_SIZE)
    val delta = Vect(scaled(HALF_SQUARE_SIZE), scaled(HALF_SQUARE_SIZE))
    (from + delta, to + delta)
  }

  def algeb4tovect(algeb4: String): Tuple2[Vect, Vect] = {
    val m = move4.move4fromalgeb(algeb4)
    val fromsq = m.from.rot(flip)
    val from = Vect(fromsq.file, fromsq.rank) * scaled(SQUARE_SIZE)
    val tosq = m.to.rot(flip)
    val to = Vect(tosq.file, tosq.rank) * scaled(SQUARE_SIZE)
    val delta = Vect(scaled(HALF_SQUARE_SIZE), scaled(HALF_SQUARE_SIZE))
    (from + delta, to + delta)
  }

  def algebxy(x: Double, y: Double): String =
    {
      if (numplayers == 2) {
        if (flip == 1) return algebfr(b.LAST_FILE - coordindex(x), b.LAST_RANK - coordindex(y))
        return algebfr(coordindex(x), coordindex(y))
      }
      algebfr(coordindex(x), coordindex(y))
    }

  def resetstate {
    ispromoting = false
  }

  def sendtables(which: String = "all") {
    if (IS_MAIN) {
      globals.ts.sajs ! SendTablesMessage(which)
    }
  }

  def storepresdone(sa: JSActor, success: Boolean) {
    val status = (if (success) "ok" else "failed")
    log("presentation upload " + status, status)

    turncoveroff

    if (success) selecttab("pres")

    drawg
  }

  def setfromtable(k: String, t: Table, receivedwebboardid: String) {
    if (receivedwebboardid != webboardid) return
    if (t == null) return
    if (!alreadyloaded) println("board registered and default table loaded")
    alreadyloaded = true
    val oldterminated = terminated
    val oldfen = table.fen
    tableid = k
    table = t
    b = new board(variant)
    b.set_from_fen(t.fen)
    draw
    resetstate
    turncoveroff
    if (terminated) {
      if ((numplayers == 2) && (meseated)) {
        var sound = "victory"
        if ((t.result == "1 - 0") && (useri == 1)) sound = "defeat"
        if ((t.result == "0 - 1") && (useri == 0)) sound = "defeat"
        if (t.result == "1/2 - 1/2") sound = "draw"
        playsound(sound)
      }
    } else if (!inprogress) {
      playsound("move")
    } else if (table.fen != oldfen) {
      playsound("move")
    }
    if (IS_FLICK) {
      speedfactor = SUPPORTED_TIME_CONTROL_PROPERTIES(table.timecontrol).flickspeedfactor
      maxspeed = SUPPORTED_TIME_CONTROL_PROPERTIES(table.timecontrol).flickmaxspeed
      if (b.flickid != "none") {
        simulate

        setTimeout(1000) { makeaimove }
      }
    }
    setTimeout(1000) { sendtables() }
  }

  def sitplayerresult(success: Boolean, receivedwebboardid: String) {
    if (receivedwebboardid != webboardid) return
    if (success) playsound("newchallenge") else playsound("newpm")
    draw
    sendtables()
    sajs ! SendTableMessage(tableid)
  }

  val dummyhandler: MouseHandler = (e: dom.MouseEvent) => {
    e.preventDefault
    val dummy = board
  }

  val boardmousemovehandler: MouseHandler = (e: dom.MouseEvent) => {
    if (aarrowon) {
      val truefilecoord = atruex(e.clientX)
      val truerankcoord = atruey(e.clientY)
      atofile = truecoordindex(truefilecoord)
      atorank = truecoordindex(truerankcoord)

      val arrow = Arrow(
        Vect(ascreencoord(afromfile), ascreencoord(afromrank)),
        Vect(ascreencoord(atofile), ascreencoord(atorank)),
        color = AARROW_COLOR,
        constantwidth = scaled(SQUARE_SIZE / 5.0)
      )
      aarrowdiv.
        style("top", arrow.svgorig.y + "px").
        style("left", arrow.svgorig.x + "px").
        style("width", arrow.pg.size.x + "px").
        style("height", arrow.pg.size.y + "px").
        style("opacity", AARROW_OPACITY).
        style("z-index", "250").
        html(arrow.svg)
    }
    if (dragunderway) {
      clearaarrow

      s(draggedid).
        style("left", truepiececoord(truex(e.clientX)) + "px").
        style("top", truepiececoord(truey(e.clientY)) + "px")
      if (IS_FLICK) {
        val arrow = Arrow(Vect(finalx, finaly), Vect(origx, origy))
        boardarrowdiv.
          style("top", (arrow.svgorig.y + scaled(HALF_SQUARE_SIZE - PADDING)) + "px").
          style("left", (arrow.svgorig.x + scaled(HALF_SQUARE_SIZE - PADDING)) + "px").
          style("width", arrow.pg.size.x + "px").
          style("height", arrow.pg.size.y + "px").
          style("opacity", "0.5").
          style("z-index", "250").
          html(arrow.svg)
      }
    }
    val dummy = board
  }

  def ascreencoord(i: Int): Double = {
    val tcoord = if (flip == 0) i else b.LAST_FILE - i
    scaled(tcoord * SQUARE_SIZE + HALF_SQUARE_SIZE)
  }

  def setarrowon {
    aarrowon = true
  }

  def setarrowoff {
    aarrowon = false

    clearaarrow
  }

  def isvalidindex(i: Int) = ((i >= 0) && (i <= b.LAST_FILE))

  def addarrow {
    val fromalgeb = square.toAlgeb(square.fromFileRank(afromfile, afromrank))
    val toalgeb = square.toAlgeb(square.fromFileRank(atofile, atorank))

    val aalgeb = fromalgeb + toalgeb

    val tfen = g.report_trunc_fen
    val bpos = g.book.get(tfen)

    bpos.addarrowalgeb(aalgeb)
  }

  var boardmousedownhandler: FullMouseHandler = (de: dom.raw.HTMLElement, e: dom.MouseEvent) => {
    if (IS_ANALYSIS && (!aarrowon)) {
      val truefilecoord = atruex(e.clientX)
      val truerankcoord = atruey(e.clientY)
      afromfile = truecoordindex(truefilecoord)
      afromrank = truecoordindex(truerankcoord)

      if ((isvalidindex(fromfile)) && (isvalidindex(fromrank))) {
        setarrowon
      }
    } else if (IS_ANALYSIS && aarrowon) {
      setarrowoff

      candd(() => addarrow, EditAction)
    }
    val dummy = board
  }

  def submitflickmove {
    turncoveron

    if (b.flickturn == "white") b.flickturn = "black" else b.flickturn = "white"
    sajs ! SendMoveMessage(tableid, b.report_fen)
  }

  val boardpromotionhandler: FullMouseHandler = (de: dom.raw.HTMLElement, e: dom.MouseEvent) => {
    val parts = de.id.split(" ")
    val f = parts(1).charAt(0)
    if (f == 'x') {
      ispromoting = false
      draw
    } else {
      val fullalgeb = if (!IS_FOUR_PLAYER) promotionalgeb + f else promotionalgeb4 + f
      ispromoting = false
      b.makeAlgebMove(fullalgeb)
      draw
      turncoveron
      sajs ! SendMoveMessage(tableid, fullalgeb)
    }
  }

  def makeandanalyzealgeb(algeb: String) {
    g.makeAlgebMove(algeb)
    if (enginerunning) analyze
  }

  def makeandanalyzesan(san: String) {
    val algeb = g.b.sanToMove(san).toAlgeb
    makeandanalyzealgeb(algeb)
  }

  def analyze {
    stopanalyzing
    enginesocket = EngineSocketActorJS(this, analyzecallback)
  }

  def analyzecallback() {
    val fen = g.report_fen
    thinkingoutput = ThinkingOutput()
    enginesocket.sendMsg(EngineMessage(action = "issue", command = "position fen " + fen))
    enginesocket.sendMsg(EngineMessage(action = "issue", command = "go infinite"))
    enginerunning = true
  }

  val boardmouseuphandler: MouseHandler = (e: dom.MouseEvent) => {
    if (dragunderway) {
      val truefilecoord = truex(e.clientX + HALF_SQUARE_SIZE - PADDING)
      val truerankcoord = truey(e.clientY + HALF_SQUARE_SIZE - PADDING)
      tofile = coordindex(truefilecoord)
      torank = coordindex(truerankcoord)
      val tosqalgeb = algebxy(truefilecoord, truerankcoord)
      val algeb = draggedalgeb + tosqalgeb
      if (IS_FOUR_PLAYER) {
        val tosqalgeb4 = square4(tofile, torank).rot(-userflip).toalgeb
        val algeb4 = draggedalgeb4 + tosqalgeb4
        val legal = b.b4.isAlgebLegal(algeb4)
        if (legal && (b.b4.isAlgebPromotion(algeb4))) {
          ispromoting = true
          promotionalgeb4 = algeb4
          draw
        } else if (!legal) {
          s(draggedid).
            style("left", origx + "px").
            style("top", origy + "px").
            style("z-index", "50")
        } else {
          b.makeAlgebMove(algeb4)
          draw
          turncoveron
          sajs ! SendMoveMessage(tableid, algeb4)
        }
      } else if (!IS_FLICK) {
        if (b.isAlgebPromotion(algeb)) {
          ispromoting = true
          promotionalgeb = algeb
          draw
        } else if (!b.isAlgebLegal(algeb)) {
          s(draggedid).
            style("left", origx + "px").
            style("top", origy + "px").
            style("z-index", "50")
        } else {
          if (!IS_ANALYSIS) {
            b.makeAlgebMove(algeb)
            draw
            turncoveron
            sajs ! SendMoveMessage(tableid, algeb)
          } else {
            candd(() => makeandanalyzealgeb(algeb), EditAction)
          }
        }
      } else {
        clearboardarrow

        val dx = flickcoordindex(origx - finalx)
        val dy = flickcoordindex(origy - finaly)

        val vx = correctspeed(speedfactor * dx)
        val vy = correctspeed(speedfactor * dy)

        b.flickid = draggedfpid

        b.flickvx = if (flip == 1) -vx else vx
        b.flickvy = if (flip == 1) -vy else vy

        submitflickmove
      }
    }
    dragunderway = false
  }

  val boarddragstarthandler: DragHandler = (de: dom.raw.HTMLElement, e: dom.DragEvent) => {
    setarrowoff

    val sel = s(de.id)
    origx = getpx(sel.style("left"))
    origy = getpx(sel.style("top"))
    offsetx = getpx(sel.style("left")) - (e.clientX - boardleft)
    offsety = getpx(sel.style("top")) - (e.clientY - boardtop)
    sel.style("z-index", "100")
    e.preventDefault()
    if ((!simulationunderway) && (!ispromoting)) {

      var legal = true

      draggedid = de.id

      if (IS_FLICK) {
        draggedfpid = getfpid(draggedid)
        val color = if (piece.colorOf(b.flickpieces.pieces(draggedfpid).piece) == piece.WHITE) "white" else "black"
        if (color != b.flickturn) legal = false
      } else {
        draggedalgeb = algebxy(origx, origy)
        val truefilecoord = truex(e.clientX + HALF_SQUARE_SIZE - PADDING)
        val truerankcoord = truey(e.clientY + HALF_SQUARE_SIZE - PADDING)
        fromfile = coordindex(truefilecoord)
        fromrank = coordindex(truerankcoord)
        draggedalgeb4 = square4(fromfile, fromrank).rot(-userflip).toalgeb
      }

      if (legal) {
        dragunderway = true
      }
    }
  }

  def clear {
    boardcontainer.html("")
  }

  def startping {
    val echo = "ws://localhost:9000/ws/ping"

    def nowtime = new js.Date().getTime()

    val socket = new dom.WebSocket(echo)
    socket.onmessage = {
      (e: dom.MessageEvent) =>
        val msg = e.data.toString
        val parts = msg.split(" ").toList
        try {
          val pingtime = parts(1).toDouble
          val difftime = nowtime - pingtime
        } catch { case e: Throwable => {} }
        setTimeout(2000) { send }
    }
    def send {
      socket.send("ping " + nowtime)
    }
    socket.onopen = { (e: dom.Event) =>
      send
    }
  }

  def flipi(i: Int): Int =
    {
      if (numplayers == 2) {
        if (flip == 1) return i
        return lastplayer - i
      }
      i
    }

  def turn: Int = {
    if (IS_FLICK) {
      if (b.flickturn == "white") return 0
      return 1
    }
    if (numplayers == 2) {
      if (b.turn == piece.WHITE) return 0
      return 1
    }
    b.b4.turn4
  }

  def dectime {
    if (inprogress) {
      player(turn).dectime(CLOCK_STEP)
      drawplayerinfo(this, turn)
    }
    setTimeout(1000) { dectime }
  }

  def checkidle {
    if (IS_MAIN) {
      if (!globals.ts.hastable(tableid)) {
        idlecount += 1
        if (idlecount >= 3) {
          sajs ! RegisterWebBoardMessage(webboardid, "default", user)
          idlecount = 0
        }
      } else {
        idlecount = 0
      }
    }

    setTimeout(10000) { checkidle }
  }

  def playerschanged {
    turncoveron
  }

  def playmousedownhandler(i: Int): MouseHandler = (e: dom.MouseEvent) => {
    if (anon) {
      global.alert("Sign in or register to be able to sit!")
    } else {
      val newplayer = player(i).copy(
        handle = user,
        kind = "human"
      )
      sajs ! SitPlayerMessage(tableid, i, newplayer)
      playerschanged
    }
    val dummy = board
  }

  case object EditAction
  case object MoveAction
  case object EditMoveAction

  def candd(action: () => Unit, kind: Any = "") {

    autosavenotes

    /////////////////////////    
    kind match {
      case EditAction => {
        if (preseditable) action()
      }
      case MoveAction => {
        action()
        if (enginerunning) analyze
      }
      case EditMoveAction => {
        if (preseditable) {
          action()
          if (enginerunning) analyze
        }
      }
      case _ => action()
    }
    /////////////////////////

    actualizepres

    presentation.version += 1

    if (presmine) {
      log("storing locally presentation version " + presentation.version)
      dom.window.localStorage.setItem(presentation.id, upickle.default.write[Presentation](presentation))
    }

    /////////////////////////
    drawg
    /////////////////////////

  }

  def gdel {
    val tfenbeforedel = g.report_trunc_fen
    g.book.delete(tfenbeforedel)
    val san = g.current_node.genSan
    g.delete
    val tfenafterdel = g.report_trunc_fen
    val bpos = g.book.get(tfenafterdel)
    bpos.delete(san)
  }

  def store {
    stopanalyzing
    val algeb = thinkingoutput.bestmovealgeb
    if (algeb != "") {
      if (g.b.isAlgebLegal(algeb)) {
        val san = g.b.toSan(smartchess.move(fromalgeb = algeb))
        val tfen = g.report_trunc_fen
        val bpos = g.book.get(tfen)
        val bm = bpos.get(san)
        bm.hasscore = true
        bm.scorecp = thinkingoutput.scorecp
        bm.scoremate = thinkingoutput.scoremate
        bm.score = thinkingoutput.score
        bm.depth = thinkingoutput.depth
      }
    }
  }

  def search(setsearchmoves: String = "") {
    stopanalyzing

    var searchmoves = setsearchmoves

    if (searchmoves == "") {

      val tfen = g.report_trunc_fen
      val bpos = g.book.get(tfen)
      val balgebs = (for (san <- bpos.moves.keys) yield g.b.sanToMove(san).toAlgeb).toList
      g.b.genMoveList
      val algebs = g.b.move_list_algebs
      val falgebs = algebs.filter(!balgebs.contains(_))
      searchmoves = falgebs.mkString(" ")

    }

    val fen = g.report_fen
    thinkingoutput = ThinkingOutput()
    enginesocket.sendMsg(EngineMessage(action = "issue", command = "position fen " + fen))
    enginesocket.sendMsg(EngineMessage(action = "issue", command = "go infinite searchmoves " + searchmoves))
    enginerunning = true
  }

  def acontrolmousedownhandler(ac: Acontrol): MouseHandler = (e: dom.MouseEvent) => {
    ac.function match {
      case "tobegin" => candd(() => g.tobegin, MoveAction)
      case "back" => candd(() => g.back, MoveAction)
      case "forward" => candd(() => g.forward, MoveAction)
      case "toend" => candd(() => g.toend, MoveAction)
      case "del" => candd(() => gdel, EditMoveAction)
      case "start" => candd(() => analyze)
      case "stop" => candd(() => stopanalyzing)
      case "make" => candd(() => make)
      case "store" => candd(() => store, EditAction)
      case "search" => candd(() => search())
      case _ => println("This was unexpected.")
    }
    val dummy = board
  }

  def aimousedownhandler(i: Int): MouseHandler = (e: dom.MouseEvent) => {
    if (anon) {
      global.alert("Sign in or register to be able to sit!")
    } else {
      val newplayer = player(i).copy(
        handle = "Bot",
        kind = "ai"
      )
      sajs ! SitPlayerMessage(tableid, i, newplayer)
      playerschanged
    }
    val dummy = board
  }

  def standmousedownhandler(i: Int): MouseHandler = (e: dom.MouseEvent) => {
    if (inprogress) {
      turncoveron
      sajs ! SendMoveMessage(tableid, "resign")
    } else {
      val newplayer = player(i).copy(
        handle = "",
        kind = "human"
      )
      sajs ! SitPlayerMessage(tableid, i, newplayer)
      playerschanged
    }
    val dummy = board
  }

  val flipbuttonmousedownhandler: MouseHandler = (e: dom.MouseEvent) => {
    flip += 1
    if (flip > lastplayer) flip = 0
    draw
    val dummy = board
  }

  def trueindex(file: Int, rank: Int): Int =
    {
      if (numplayers == 2) {
        if (flip == 0) return index(file, rank)
        return index(b.LAST_FILE - file, b.LAST_RANK - rank)
      }
      index(file, rank)
    }

  def turncoveron {
    coveron = true
    draw
    setTimeout(10000) {
      coveron = false
      draw
    }
  }

  def turncoveroff {
    coveron = false
    draw
  }

  def timestep = 0.01 // time step of simulation, seconds
  def maxsteps = 10000 // maximum number of steps simulated
  var speedfactor = 20.0 // initial speed relative to drag [flickcoord/sec]
  var maxspeed = 20.0 // maximal initial speed
  def frictionconstant = 1.5 // friction force in function of speed
  def springconstant = 1000.0 // constant used for Hooke's law  
  def closedistance = 0.8 // distance under which elastic collision takes place

  def speedsettledlimit = 0.5 // speed under which motion considered to be settled
  def distancesettledlimit = closedistance // dont stop while pieces are too close

  var settled = false // to keep track wether movement has settled
  var step = 0 // simulation step

  def fallenlimit = 1.0
  def flicksize = 7.0
  def fallen(p: FlickPiece) = (p.x > flicksize + fallenlimit) || (p.x < -fallenlimit) || (p.y > flicksize + fallenlimit) || (p.y < -fallenlimit)

  def frictionforce(v: Double, m: Double) = -frictionconstant * m * v
  def deltar(r: Double, v: Double, m: Double, f: Double) = r + v * timestep + 0.5 * f / m * timestep * timestep
  def deltav(v: Double, m: Double, f: Double) = v + f / m * timestep
  def dist(x1: Double, y1: Double, x2: Double, y2: Double) = scala.math.sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))

  def abs(d: Double) = if (d < 0.0) -d else d

  def correctspeed(v: Double) = if (v < -maxspeed) -maxspeed else if (v > maxspeed) maxspeed else v

  def getflickresult: String = {
    var flickresult = "none"
    var numwhite = 0
    var numblack = 0
    for ((k, v) <- fps) {
      if (v.piece != piece.NO_PIECE) if (!fallen(v))
        if (piece.colorOf(v.piece) == piece.WHITE) numwhite += 1 else numblack += 1
    }
    val blacklost = (numblack == 0)
    val whitelost = (numwhite == 0)
    if (whitelost || blacklost) {
      flickresult = if (whitelost && blacklost) "1/2 - 1/2" else if (blacklost) "1 - 0" else "0 - 1"
    }
    flickresult
  }

  def simulationstep {

    var anyspeedabovesettledlimit = false

    var anydistancebelowsettledlimit = false

    def elasticforce(p: FlickPiece, coord: String): Double = {
      var sumforce = 0.0
      for ((k, v) <- fps) {
        if ((v.id != p.id) && !fallen(v)) {
          val d = dist(p.x, p.y, v.x, v.y)
          if (d < closedistance) {
            anydistancebelowsettledlimit = true
            val c1 = if (coord == "x") p.x else p.y
            val c2 = if (coord == "x") v.x else v.y
            val effd = (closedistance - d)
            val f = (c1 - c2) * effd * springconstant
            sumforce += f
          }
        }
      }
      sumforce
    }

    for ((k, v) <- fps) {

      if (!fallen(v)) {
        val fx = frictionforce(v.vx, v.m) + elasticforce(v, "x")
        val fy = frictionforce(v.vy, v.m) + elasticforce(v, "y")

        v.x = deltar(v.x, v.vx, v.m, fx)
        v.y = deltar(v.y, v.vy, v.m, fy)

        v.vx = deltav(v.vx, v.m, fx)
        v.vy = deltav(v.vy, v.m, fy)

        if ((abs(v.vx) > speedsettledlimit) || (abs(v.vy) > speedsettledlimit)) anyspeedabovesettledlimit = true
      }

      val sel = s(flickpieceid(k))
      sel.
        style("left", px(flickscreencoord(v.x))).
        style("top", px(flickscreencoord(v.y))).
        style("visibility", if (fallen(v)) "hidden" else "visible")
    }
    step += 1

    settled = (!anyspeedabovesettledlimit) && (!anydistancebelowsettledlimit) || (!simulationunderway)

    if (!settled && (step < maxsteps)) setTimeout(timestep * 1000.0) { simulationstep } else {

      simulationunderway = false

      if (!terminated) {

        val flickresult = getflickresult

        if (flickresult != "none") {

          report_flickresult(flickresult)

        }

      }

    }
  }

  var simulationunderway = false

  var r = new scala.util.Random()

  def makeaimove {
    if (!inprogress) return

    if (isUsersTurn) return

    if ((player(0).kind != "ai") && (player(1).kind != "ai")) return

    if (!meseated) return

    val flickresult = getflickresult

    if (flickresult != "none") {
      report_flickresult(flickresult)
      simulationunderway = false
      return
    }

    if (simulationunderway) setTimeout(1000) { makeaimove }

    val aicolor = if (player(0).kind == "ai") piece.WHITE else piece.BLACK

    val apids = (for ((k, v) <- fps if ((piece.colorOf(v.piece) == aicolor)) && !fallen(v)) yield v.id).toList

    val rid = apids(r.nextInt(apids.length))

    b.flickid = rid
    b.flickvy = correctspeed(20.0)
    b.flickvx = 0.0

    submitflickmove
  }

  def fps = b.flickpieces.pieces

  def simulate {
    if (b.flickid == "none") return

    val flickresult = getflickresult

    if (flickresult != "none") {
      report_flickresult(flickresult)
      simulationunderway = false
      return
    }

    if (simulationunderway) {
      simulationunderway = false
      setTimeout(500) {
        simulate
      }
    } else {
      simulationunderway = true

      // set up initial conditions

      for ((k, v) <- fps) {
        if (k == b.flickid) {
          // initial condition for shooted piece
          v.vx = b.flickvx
          v.vy = b.flickvy
        } else {
          // initial condition for other pieces
          v.vx = 0.0
          v.vy = 0.0
        }
      }

      step = 0

      simulationstep
    }
  }

  def getnodeurl: String = {
    val testg = new game(g.variant)
    testg.set_from_pgn(g.report_pgn)
    for (algeb <- g.current_line_moves_algeb) testg.makeAlgebMove(algeb)

    prurl + "--" + testg.get_current_node_nodeid
  }

  var donodeonce = false

  def pres_html(pr: Presentation, g: game): Tuple2[String, List[String]] = {
    val ids = scala.collection.mutable.ArrayBuffer[String]()

    ////////////////////////////////////////////////////////

    val DO_NODE = false

    val td = """td class="prestd""""
    val td2 = """td class="prestd" colspan="2""""

    ////////////////////////////////////////////////////////
    // ids

    val gid = "gennewpresidbutton"
    val fid = "flippresbutton"
    val uid = "uploadpresbutton"
    val aid = "archivepresbutton"
    val hid = "hybernatepresbutton"

    ////////////////////////////////////////////////////////
    // buttoncontents

    ids += gid

    val flippresbuttoncontent = if (preseditable) {
      ids += fid
      s"""<input type="button" id="$fid" value="${t("ccfg.flip")}">"""
    } else ""

    var uploadpresbuttoncontent = if (preseditable) {
      ids += uid
      s"""<$td><input type="button" id="$uid" value="${t("ccfg.upload")}"></td>"""
    } else s"""<$td></td>"""

    val archivepresbuttoncontent = if (preseditable) {
      ids += aid
      s"""<input type="button" id="$aid" value="${t("presentation.changearchived")}">"""
    } else ""

    val hybernatepresbuttoncontent = if (presmine) {
      ids += hid
      s"""<input type="button" id="$hid" value="${t("presentation.changehybernated")}">"""
    } else ""

    ////////////////////////////////////////////////////////

    val nodeurlandlink = if (DO_NODE || donodeonce) {
      val nodeurl = getnodeurl

      donodeonce = false

      s"""
      |<tr>
      |<$td>${t("ccfg.nodeurl")}</td>
      |<$td2><input type="text" class="presurltext" value="${nodeurl}"></td>
      |</tr>
      |
      |<tr>
      |<$td>${t("ccfg.nodelink")}</td>      
      |<$td2><a href="${nodeurl}">${nodeurl}</a></td>
      |</tr>
    """.stripMargin
    } else {
      ids += "createnodebutton1"
      ids += "createnodebutton2"

      s"""
      |<tr>
      |<$td>${t("ccfg.nodeurl")}</td>
      |<$td2><input type="button" id="createnodebutton1" value="${t("ccfg.create")}"></td>
      |</tr>
      |
      |<tr>
      |<$td>${t("ccfg.nodelink")}</td>
      |<$td2><input type="button" id="createnodebutton2" value="${t("ccfg.create")}"></td>      
      |</tr>
    """.stripMargin
    }

    val enginescontent = (for (e <- availableengines) yield {
      val selected = if (e == pr.enginename) " selected" else ""
      s"""<option value="$e"$selected>$e</option> """
    }).mkString("\n")

    val enginecontent = s"""
      |<select id="selectengine">
      |$enginescontent
      |</select>
    """.stripMargin

    ////////////////////////////////////////////////////////

    val content = s"""
      |<table>
      |
      |<tr>
      |<$td>${t("ccfg.current")}</td>
      |<$td2><input type="text" class="prescurrenttext" value="${g.current_line_pgn}"></td>      
      |</tr>
      |
      |<tr>
      |<$td>${t("ccfg.flip")}</td>
      |<$td>$flippresbuttoncontent</td>
      |<$td>${if (pr.flip) t("ccfg.yes") else t("ccfg.no")}</td>
      |</tr>
      |
      |<tr>
      |<$td>${t("ccfg.id")}</td>
      |<$td><input type="button" id="$gid" value="${t("ccfg.gennew")}"></td>
      |<$td>${pr.id}</td>
      |</tr>
      |
      |<tr>
      |<$td>Url</td>
      |$uploadpresbuttoncontent
      |<$td><input type="text" class="presurltext" value="${prurl}"></td>
      |</tr>
      |
      |<tr>
      |<$td>Link</td>      
      |<$td2><a href="${prurl}">${prurl}</a></td>
      |</tr>
      |
      |$nodeurlandlink
      |
      |<tr>
      |<$td>${t("ccfg.title")}</td>
      |<$td2><input type="text" style="width:400px;" id="prtitle" value="${pr.title}"></td>
      |</tr>
      |
      |<tr>
      |<$td>${t("ccfg.owner")}</td>
      |<$td2>${pr.owner}</td>      
      |</tr>
      |
      |<tr>
      |<$td>${t("presentation.archive")}</td>
      |<$td>$archivepresbuttoncontent</td>
      |<$td>${if (pr.candelete == "no") t("presentation.archived") else t("presentation.notarchived")}</td>
      |</tr>
      |
      |<tr>
      |<$td>${t("presentation.hybernate")}</td>
      |<$td>$hybernatepresbuttoncontent</td>
      |<$td>${if (pr.canedit == "no") t("presentation.hybernated") else t("presentation.nothybernated")}</td>
      |</tr>
      |
      |<tr>
      |<$td>${t("ccfg.version")}</td>
      |<$td2>${pr.version}</td>      
      |</tr>
      |
      |<tr>
      |<$td>${t("presentation.engine")}</td>
      |<$td>$enginecontent</td>      
      |<$td>${pr.enginename}</td>      
      |</tr>
      |
      |</table>
    """.stripMargin

    ////////////////////////////////////////////////////////

    (content, ids.toList)
  }

  def book_html(g: game): Tuple2[String, List[String]] = {
    val ids = scala.collection.mutable.ArrayBuffer[String]()

    ////////////////////////////////////////////////////////

    val tfen = g.report_trunc_fen
    val bpos = g.book.get(tfen)

    val td = """td class="booktd""""
    val tdv = """td class="booktd" style="vertical-align:top;""""

    ////////////////////////////////////////////////////////

    val movescontent = (for (san <- bpos.movessorted) yield {
      val bm = bpos.get(san)

      val emid = "evalmove_" + san + "_none"

      ids += emid

      var annotscontent = (for (annot <- ANNOTS) yield {
        val ap = ANNOT_PROPERTIES(annot)
        val col = ap.col
        val id = "annotcontrol_" + san + "_" + annot
        val emid = "evalmove_" + san + "_none"

        if (preseditable) ids += id

        val annotcontent = if (preseditable)
          s"""
          |<$tdv><span class="bookannotcontrol" id="$id"><font color="$col">$annot</font></span></td>
        """.stripMargin
        else s"""<$td></td>"""

        s"""
          |$annotcontent          
        """.stripMargin
      }).mkString("\n")

      val annot = bm.annot
      val ap = ANNOT_PROPERTIES(annot)
      val col = ap.col
      val id = "makemove_" + san + "_none"
      ids += id

      val ncid = "movenotecontrol_" + san + (if (bm.open) "_save" else "_edit")
      val nid = "movenote" + san

      val movecommentcontent = if (bm.open) if (preseditable) s"""
          |<textarea id="$nid" rows="3" cols="30">${bm.comment}</textarea>
        """.stripMargin
      else ""
      else if (bm.comment != "") s"""          
          |<div style="border-style:solid; border-width:1px; padding:5px; border-radius:10px; border-color: #7f7f7f;">
          |${bm.comment}
          |</div>
        """.stripMargin
      else ""

      val moveno = g.b.fullmove_number + (if (g.b.turn == smartchess.piece.BLACK) ".." else ".")

      val editbuttoncontent = if (preseditable) {
        ids += ncid
        s"""
          |<input type="button" id="$ncid" value="${if (bm.open) t("ccfg.savemovenote") else t("ccfg.editmovenote")}">
        """.stripMargin
      } else ""

      s"""
        |<tr>
        |<$td><span class="booksan"><font color="$col">$moveno</font></span></td>
        |<$td><span class="booksan"><font color="$col" id="$id">$san</font></span></td>        
        |<$td><span class="bookannot"><font color="$col">${if (annot == "-") "" else annot}</font></span></td>
        |<$td><span id="$emid" style="cursor:pointer;font-size:23px;font-weight:bold;"><font color="$col">${if (bm.hasscore) bm.scoreformatted else "eval"}</font></span></td>
        |<$td><span><font color="$col" style="font-size:12px;color:#00007f">${if (bm.hasscore) bm.depth else ""}</font></span></td>
        |${if (preseditable) annotscontent else ""}
        |<$td></td>
        |<$tdv>$movecommentcontent</td>
        |<$tdv>$editbuttoncontent</td>
        |</tr>
      """.stripMargin
    }).mkString("\n")

    ////////////////////////////////////////////////////////

    val content = s"""
      |<table>
      |$movescontent
      |</table>
    """.stripMargin

    ////////////////////////////////////////////////////////

    (content, ids.toList)
  }

  def import_html(g: game): Tuple2[String, List[String]] = {
    val ids = scala.collection.mutable.ArrayBuffer[String]()

    ////////////////////////////////////////////////////////
    // ids

    val iid = "importpgnbutton"

    ////////////////////////////////////////////////////////
    // buttoncontents

    val importpgnbuttoncontent = if (preseditable) {
      ids += iid
      s"""<input type="button" id="$iid" value="${t("ccfg.importpgn")}">"""
    } else ""

    ////////////////////////////////////////////////////////

    val content = s"""
      |<table>      
      |<tr>
      |<td>$importpgnbuttoncontent</td>
      |</tr>
      |<tr>
      |<td><textarea rows="25" cols="70" id="importpgntextarea">${g.report_pgn}</textarea></td>
      |</tr>
      |</table>
    """.stripMargin

    ////////////////////////////////////////////////////////

    (content, ids.toList)
  }

  def notes_html(pr: Presentation, g: game): Tuple2[String, List[String]] = {
    val ids = scala.collection.mutable.ArrayBuffer[String]()

    ////////////////////////////////////////////////////////

    val tfen = g.report_trunc_fen
    val bpos = g.book.get(tfen)

    ////////////////////////////////////////////////////////
    // ids

    val sid = "savenotesbutton"

    ////////////////////////////////////////////////////////
    // buttoncontents

    val savenotesbuttoncontent = if (preseditable) {
      ids += sid
      s"""<input type="button" id="$sid" value="${t("ccfg.savenotes")}">"""
    } else ""

    ////////////////////////////////////////////////////////

    val content = s"""
      |<table>      
      |<tr>
      |<td>$savenotesbuttoncontent</td>
      |</tr>
      |<tr>
      |<td><textarea rows="25" cols="70" id="notestextarea">${bpos.notes}</textarea></td>
      |</tr>
      |</table>
    """.stripMargin

    ////////////////////////////////////////////////////////

    (content, ids.toList)
  }

  def essay_html(pr: Presentation, g: game): Tuple2[String, List[String]] = {
    val ids = scala.collection.mutable.ArrayBuffer[String]()

    ////////////////////////////////////////////////////////
    // ids

    val sid = "saveessaybutton"

    ////////////////////////////////////////////////////////
    // buttoncontents

    val saveessaybuttoncontent = if (preseditable) {
      ids += sid
      s"""<input type="button" id="$sid" value="${t("ccfg.saveessay")}">"""
    } else ""

    ////////////////////////////////////////////////////////

    val content = s"""
      |<table>      
      |<tr>
      |<td>$saveessaybuttoncontent</td>
      |</tr>
      |<tr>
      |<td><textarea rows="25" cols="70" id="essaytextarea">${g.book.essay}</textarea></td>
      |</tr>
      |</table>
    """.stripMargin

    ////////////////////////////////////////////////////////

    (content, ids.toList)
  }

  def pgn_html(pr: Presentation, g: game): Tuple2[String, List[String]] = {
    val pgnhtml = g.report_pgn_html(cn = g.current_node, doheaders = false)

    ////////////////////////////////////////////////////////

    val content = s"""
      |<div id="pgnhtmlcontent" style="padding:10px; line-height:225%;">
      |${pgnhtml._1}
      |</div>
    """.stripMargin

    ////////////////////////////////////////////////////////

    (content, pgnhtml._2)
  }

  def getinputtext(id: String): String = {
    dgebid(id).value.toString()
  }

  val AUTO_SAVE_NOTES = true

  def actualizepres {

    if (!preseditable) return

    presentation.pgn = g.report_pgn
    val title = getinputtext("prtitle")
    presentation.title = title
    val cla = g.current_line_algeb
    presentation.currentlinealgeb = cla

  }

  def autosavenotes {

    if (!preseditable) return

    if (AUTO_SAVE_NOTES) {
      savenotes()
      saveessay()
    }

  }

  def uploadpres {

    presgame.clearplayers

    autosavenotes

    actualizepres

    selecttab("log")

    log("uploading presentation -- " + presentation.title + " --", "warn")

    turncoveron

    val ua = PresentationUploadActorJS(this)

    log("opening socket")

  }

  def setpflip {
    if (presentation.flip) flip = 1 else flip = 0
  }

  def flippres {
    presentation.flip = !presentation.flip

    setpflip
  }

  def togglearchivepres {
    if (presentation.candelete != "no") { presentation.candelete = "no" } else { presentation.candelete = "" }
    uploadpres
  }

  def togglehybernatepres {
    if (presentation.canedit != "no") {
      presentation.canedit = "no"
      presentation.candelete = "no"
    } else { presentation.canedit = "" }
    uploadpres
  }

  def setdonodeonce {
    donodeonce = true
  }

  def presbuttoncallback(bid: String) {
    bid match {
      case "gennewpresidbutton" => candd(() => gennewpresid)
      case "uploadpresbutton" => uploadpres
      case "flippresbutton" => candd(() => flippres, EditAction)
      case "createnodebutton1" => candd(() => setdonodeonce)
      case "createnodebutton2" => candd(() => setdonodeonce)
      case "archivepresbutton" => candd(() => togglearchivepres)
      case "hybernatepresbutton" => candd(() => togglehybernatepres)
    }
  }

  def bookbuttoncallback(acid: String) {
    val parts = acid.split("_").toList

    val command = parts(0)
    val san = parts(1)
    val annot = parts(2)

    val tfen = g.report_trunc_fen
    val bpos = g.book.get(tfen)

    def annotsan {
      val bm = bpos.get(san)

      bm.annot = annot
    }

    if (command == "annotcontrol") {
      candd(() => annotsan, EditAction)
    }

    if (command == "makemove") {
      candd(() => makeandanalyzesan(san))
    }

    if (command == "movenotecontrol") {
      val bm = bpos.get(san)

      def savemovenote {
        val nid = "movenote" + san
        val content = getinputtext(nid)
        bm.comment = content
        bm.open = !bm.open
      }

      def togglebmopen {
        bm.open = !bm.open
      }

      annot match {
        case "save" => candd(() => savemovenote, EditAction)
        case "edit" => candd(() => togglebmopen)
      }
    }

    if (command == "evalmove") {
      val searchalgeb = g.b.sanToMove(san).toAlgeb

      search(searchalgeb)
    }
  }

  def setpresfrompgn(ipgn: String = getinputtext("importpgntextarea")) {
    g = new game(globals.analysisvariantcombo.getselected)
    presentation.book = Book()
    g.book = presentation.book
    g.set_from_pgn(ipgn)
    table.variant = g.variant
    globals.analysisvariantcombo.setselected(g.variant)
    b = new board(g.variant)
  }

  def importcallback(bid: String) {
    if (bid == "importpgnbutton") {
      candd(() => setpresfrompgn(), EditAction)
    }
  }

  def savenotes(notes: String = getinputtext("notestextarea")) {
    val tfen = g.report_trunc_fen
    val bpos = g.book.get(tfen)

    bpos.notes = notes
  }

  def saveessay(essay: String = getinputtext("essaytextarea")) {
    g.book.essay = essay
  }

  def notescallback(bid: String) {
    if (bid == "savenotesbutton") {
      candd(() => savenotes(), EditAction)
    }
  }

  def essaycallback(bid: String) {
    if (bid == "saveessaybutton") {
      candd(() => saveessay(), EditAction)
    }
  }

  def pgnclickedcallback(nodeid: String) {
    val parts = nodeid.split("_").toList
    val index = parts(1).toInt

    def toclickednode {
      val gn = g.html_pgn_nodes(index)
      g.tonode(gn)
    }

    candd(() => toclickednode, MoveAction)
  }

  def selectengine(name: String) {
    presentation.enginename = name
    startengine
  }

  def presselectcallback(sid: String, value: String) {
    sid match {
      case "selectengine" => candd(() => selectengine(value))
    }
  }

  def startengine {
    enginesocket.sendMsg(EngineMessage(action = "start", name = presentation.enginename))
  }

  def stopanalyzing {
    enginesocket.sendMsg(EngineMessage(action = "issue", command = "stop"))
    enginerunning = false
  }

  def make {
    if (thinkingoutput.bestmovealgeb != "") {
      g.makeAlgebMove(thinkingoutput.bestmovealgeb)
      analyze
    }
  }

  def selecttab(tid: String) {
    globals.tabs.selecttab(tid)
  }

  def calcpgnv(pgnhtmlcontent: String): Double = {
    val mi = pgnhtmlcontent.indexOf("padding: 3px")

    val len = pgnhtmlcontent.length.toDouble

    var v = 0.0

    if (mi > 2000) {
      v = (mi - 2000) / len
    }

    v
  }

  def drawg {
    b.set_from_fen(g.report_fen)
    table.algebline = g.current_line_algeb
    draw

    val bh = book_html(g)
    globals.tabs.setcontent("book", Tabcontent(bh._1, bh._2, bookbuttoncallback))
    val gh = pgn_html(presentation, g)
    globals.tabs.setcontent("pgn", Tabcontent(gh._1, gh._2, pgnclickedcallback, vscroll = calcpgnv(gh._1)))
    val ih = import_html(g)
    globals.tabs.setcontent("import", Tabcontent(ih._1, ih._2, importcallback))
    if ((presentation.enginename == "") && (availableengines.length > 0)) presentation.enginename = availableengines(0)
    val ph = pres_html(presentation, g)
    globals.tabs.setcontent("pres", Tabcontent(ph._1, ph._2, presbuttoncallback, List("selectengine"), presselectcallback))
    val nh = notes_html(presentation, g)
    globals.tabs.setcontent("notes", Tabcontent(nh._1, nh._2, notescallback))
    val eh = essay_html(presentation, g)
    globals.tabs.setcontent("essay", Tabcontent(eh._1, eh._2, essaycallback))

    globals.tabs.draw
  }

  def gennewpresid {
    presentation.id = java.util.UUID.randomUUID.toString()
    presentation.owner = user
  }

  def setpresvariant(setvariant: String) {
    presentation.book = Book()
    g = new game(setvariant)
    g.reset
    g.book = presentation.book
    b = new board(setvariant)
  }

  def analysisvariantselectedcallback(sel: String) {
    candd(() => setpresvariant(sel), EditAction)
  }

  def log(content: String, status: String = "") {
    logger.log(Logitem(content, status))
    globals.tabs.setcontent("log", Tabcontent(logger.reportHTML))
    setTimeout(50) {
      globals.tabs.draw
    }
  }

  def uploadopened(sa: JSActor) {
    log("socket opened, uploading presentation")

    sa ! StorePresentationMessage(presentation.id, presgame)
  }

  def sendavailablecallback() {
    enginesocket.sendMsg(EngineMessage(action = "sendavailable"))
  }

  def drawenginearrow(to: ThinkingOutput) {
    if (to.bestmovealgeb == "") {
      enginearrowdiv.style("visibility", "hidden")
      enginescorediv.style("visibility", "hidden")
      enginedepthdiv.style("visibility", "hidden")
    } else {
      val av = algebtovect(to.bestmovealgeb)
      val col = if (to.score >= 0) "#00ff00" else "#ff0000"
      val arrow = Arrow(av._1, av._2, constantwidth = scaled(SQUARE_SIZE / 4.0), color = col)
      enginearrowdiv.
        style("top", (arrow.svgorig.y) + "px").
        style("left", (arrow.svgorig.x) + "px").
        style("width", arrow.pg.size.x + "px").
        style("height", arrow.pg.size.y + "px").
        style("opacity", "0.9").
        style("z-index", "" + (ARROW_ZINDEX + 1)).
        style("visibility", "visible").
        html(arrow.svg)
      enginescorediv.
        style("top", px(BOARD_HEIGHT / 4.0 + SQUARE_SIZE)).
        style("left", px(BOARD_WIDTH / 4.0)).
        style("font-size", 3.0 * SQUARE_SIZE + "px").
        style("color", col).
        style("z-index", "" + (ARROW_ZINDEX + 2)).
        style("visibility", "visible").
        html((if (to.scoremate) "#" else "") + to.score)
      enginedepthdiv.
        style("top", px(BOARD_HEIGHT / 4.0)).
        style("left", px(BOARD_WIDTH / 4.0 - SQUARE_SIZE)).
        style("font-size", 1.5 * SQUARE_SIZE + "px").
        style("color", "#0000ff").
        style("z-index", "" + (ARROW_ZINDEX + 2)).
        style("visibility", "visible").
        html("" + to.depth)
    }
  }

  def enginemessagereceived(x: EngineMessage) {
    x.action match {
      case "available" => {
        availableengines = x.available
        drawg
        startengine
      }
      case "thinkingoutput" => {
        val buffernormalized = x.buffer.replaceAll("\\r", "")
        log(buffernormalized)
        thinkingoutput.update(buffernormalized)
        drawenginearrow(thinkingoutput)
      }
    }
  }

  def createenginesocket {
    enginesocket = EngineSocketActorJS(this, sendavailablecallback)
  }

  def loadpresentation {
    createenginesocket

    selecttab("log")

    log("loading presentation", "warn")

    if (presentation.id == "") {
      log("presentation has no id, generating new")
      gennewpresid
    }

    val ls = dom.window.localStorage.getItem(presentation.id)

    if (ls != null) {
      log("presentation has locally stored version", "warn")

      val lp = upickle.default.read[Presentation](ls)

      if (presmine && (presentation.version < lp.version)) {
        log("local version higher, using locally stored version", "warn")

        chessconfig.presgame.presentation = lp
      }
    }

    g = new game
    g.book = presentation.book
    g.set_from_pgn(chessconfig.getpgn)
    table.variant = g.variant
    globals.analysisvariantcombo.setselected(g.variant)
    b = new board(g.variant)
    if (presentation.owner == "") {
      log("presentation has no owner, making owner " + user)
      presentation.owner = user
    }
    if (presentation.currentlinealgeb != "") {
      log("presentation has initial moves to make, making moves")
      val algebs = presentation.currentlinealgeb.split(" ")
      for (algeb <- algebs) g.makeAlgebMove(algeb)
    }
    setpflip
    if (chessconfig.currentnodeid >= 0) {
      g.select_node_by_nodeid(chessconfig.currentnodeid)
    }
    log("presentation -- " + presentation.title + " -- set up ok", "ok")

    drawg
  }

  var tries = 0

  def loadsomething {

    if (IS_MAIN) {
      try {
        tries += 1
        println("trying to register board and load default table, try " + tries)
        sajs ! RegisterWebBoardMessage(webboardid, "default", if (IS_MAIN) user else "")
      } catch {
        case e: Throwable => {
          println("socket not open yet")
          if (tries < 10) {
            setTimeout(3000) { loadsomething }
          } else {
            println("socket could not be opened")
          }
        }
      }
    }

    if (IS_ANALYSIS) {
      loadpresentation

      selecttab("book")
    }

  }

  def report_flickresult(flickresult: String) {

    b.flickresult = flickresult

    sajs ! SendMoveMessage(tableid, b.report_fen)
  }

  def pawndir: Tuple2[Int, Int] = {
    var filedir = 0
    var rankdir = 0
    if (!IS_FOUR_PLAYER) {
      rankdir = if (turn == 0) -1 else 1
      if (flip == 1) rankdir = -rankdir
    } else {
      val pd = PieceVector4.PAWN_DIRS(b.b4.turn4)
      filedir = pd.dfile
      rankdir = pd.drank
    }
    (filedir, rankdir)
  }

}