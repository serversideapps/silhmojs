package shared

import SharedSettings._

// shared between server and client

object SharedLogic {
  def sorttablekeyslogic(a: Table, b: Table): Boolean = {
    if (a.terminated != b.terminated) return b.terminated
    if (a.inprogress != b.inprogress) return b.inprogress
    if (a.numseated != b.numseated) return a.numseated > b.numseated
    if (a.created.toDouble != b.created.toDouble) return a.created.toDouble > b.created.toDouble
    false
  }
}

case class Setting(
    piecetype: Option[String] = Some("alpha"),
    note: Option[String] = Some("")
) {

}

case class ChessConfig(
    kind: String = "play",
    gameID: String = "none",
    var presgame: Game = Game(),
    var currentnodeid: Int = 0,
    var translations: Map[String, String] = Map[String, String]()
) {
  def presentation = presgame.presentation
  def getpgn: String = {
    if (presentation.pgn == "") return presgame.pgn
    presentation.pgn
  }
  def translate(phrase: String): String = {
    if (translations.contains(phrase)) return translations(phrase)
    phrase
  }
}

case class Player(
    var uuid: java.util.UUID = null,
    var handle: String = "",
    var rating: Double = 1200.0,
    var rd: Double = 350.0,
    var lastrated: Double = 0.0,
    var title: String = "",
    var timems: Double = 3.0 * SharedTimeUtils.MinuteMS,
    var lasttimems: Double = 3.0 * SharedTimeUtils.MinuteMS,
    var kind: String = "human",
    var startedthinkingms: Double = 0.0,
    var resigned: Boolean = false
) {
  import SharedTimeUtils._
  def minutes: Int = Math.floor(timems / MinuteMS).toInt
  def seconds: Int = ((timems - minutes.toDouble * MinuteMS) / SecondMS).toInt

  def dectime(decms: Double) {
    timems = timems - decms
    if (timems < 0.0) timems = 0.0
  }

  def formattime: String = "%02d : %02d".format(minutes, seconds)
  def lhandle(maxlength: Int = -1): String = if (maxlength < 0) handle else if (handle.length < maxlength) handle else handle.substring(0, maxlength) + ".."
  def fhandle(maxlength: Int = -1): String = if (human) lhandle(maxlength) else s"""<font color="blue">AI</font> $handle"""
  def shandle(maxlength: Int = -1): String = if (human) lhandle(maxlength) else s"""AI $handle"""
  def formattedhandle(withrating: Boolean = false, maxlength: Int = -1): String = {
    if (handle != "") s"""<b>${fhandle(maxlength)}</b> ${formattedrating}""" else ""
  }
  def valid = (handle != "")
  def human = (kind == "human")
  def ai = (kind == "ai")
  def validhuman = (valid && human)
  def formattedrating = "%.0f".format(rating)
}

case class Table(
    val id: String,
    var variant: String,
    var timecontrol: String,
    var players: Array[Player] = Array(Player(), Player(), Player(), Player()),
    var created: String = "", // create time ms
    var createdF: String = "", // create time formatted
    var inprogress: Boolean = false,
    var fen: String = "",
    var terminated: Boolean = false,
    var result: String = "",
    var resultreason: String = "",
    var valid: Boolean = true,
    var idle: Int = 0,
    var algebline: String = ""
) {
  def IS_FOUR_PLAYER = (variant == "Four Player")

  def isDuplicateOf(t: Table): Boolean = (
    (variant == t.variant) &&
    (timecontrol == t.timecontrol) &&
    (inprogress == t.inprogress) &&
    (terminated == t.terminated)
  )
  def gameinfo: String = variant + " " + timecontrol

  def getuseri(handle: String): Int =
    {
      var i = 0
      for (p <- players) { if ((p.handle == handle) && (p.valid)) return i; i += 1 }
      return -1
    }

  def hashandle(handle: String): Boolean = if (handle == "") false else (getuseri(handle) >= 0)

  def reqplayers = if (!IS_FOUR_PLAYER) 2 else 4

  def resetplayers {
    players = Array(Player(), Player(), Player(), Player())
  }
  def hasplayer(i: Int) = (players(i).valid)
  def hashuman: Boolean = {
    for (p <- players) if (p.validhuman) return true
    false
  }
  def anyseated: Boolean = {
    for (p <- players) if (p.valid) return true
    false
  }
  def numseated: Int = {
    var ns = 0
    for (p <- players) if (p.valid) ns += 1
    ns
  }
  def humanhandles: List[String] = (for (p <- players if ((p.valid) && (p.human))) yield p.handle).toList

  def ready: Boolean = {
    if (reqplayers == 2) {
      return (hasplayer(0) && hasplayer(1))
    }
    if (reqplayers == 4) {
      return (hasplayer(0) && hasplayer(1) && hasplayer(2) && hasplayer(3))
    }
    false
  }

  def statusclass: String = {
    if (terminated) return "tableTerminated"
    if (inprogress) return "tableInprogress"
    "tableOpen"
  }

  def nonresignedhumans: List[String] = (for (p <- players if ((p.valid) && (p.human) && (!p.resigned))) yield p.handle).toList
}

case class Game(
    usergameid: String = "",
    createdms: Double = 0.0,
    White: String = "",
    Black: String = "",
    Yellow: String = "",
    Red: String = "",
    Result: String = "",
    Date: String = "",
    Time: String = "",
    var player0: String = "",
    var player1: String = "",
    var player2: String = "",
    var player3: String = "",
    pgn: String = "",
    variant: String = "",
    presentationid: String = "",
    presentationowner: String = "",
    presentationcandelete: String = "",
    presentationcanedit: String = "",
    var presentationtitle: String = "",
    var presentation: Presentation = Presentation()
) {
  def isanalyzed = ANALYZED_VARIANTS.contains(variant)
  def effectiveid: String = {
    if (presentation.id == "") return usergameid
    presentation.id
  }
  def clearplayers {
    player0 = ""
    player1 = ""
    player2 = ""
    player3 = ""
  }
}

case class Presentation(
    var id: String = "",
    var title: String = "",
    var owner: String = "",
    var pgn: String = "",
    var currentlinealgeb: String = "",
    var book: Book = Book(),
    var flip: Boolean = false,
    var version: Int = 0,
    var candelete: String = "",
    var canedit: String = "",
    var enginename: String = ""
) {
  def url = if (id == "") "" else SITE_URL + "/analysis/" + id
}

case class BookMove(
    var fen: String = "",
    var san: String = "",
    var annot: String = "",
    var comment: String = "",
    var open: Boolean = true,
    var hasscore: Boolean = false,
    var scorecp: Boolean = true,
    var scoremate: Boolean = false,
    var score: Int = 0,
    var depth: Int = 0
) {
  def scoreformatted = if (scorecp) "%.1f".format(score.toDouble / 100.0) else "#" + score
}

case class BookPosition(
    var fen: String = "",
    var moves: Map[String, BookMove] = Map[String, BookMove](),
    var notes: String = "",
    var arrowalgebs: List[String] = List[String]()
) {
  def get(san: String): BookMove = {
    if (!moves.contains(san)) moves += (san -> BookMove())
    moves(san)
  }
  def getannot(san: String) = get(san).annot
  def getannotproperty(san: String) = ANNOT_PROPERTIES(getannot(san))

  def sortfunc(a: String, b: String): Boolean = {
    val ap = getannotproperty(a)
    val bp = getannotproperty(b)
    if (ap.score != bp.score) return ap.score > bp.score
    val am = moves(a)
    val bm = moves(b)
    if (am.scorecp && bm.scorecp) return am.score > bm.score
    if (am.scorecp && bm.scoremate) return bm.score < 0
    if (am.scoremate && bm.scorecp) return am.score >= 0
    am.score < bm.score
  }

  def movessorted = moves.keys.toList.sortWith(sortfunc)

  def delete(san: String) {
    if (moves.contains(san)) moves -= san
  }

  def addarrowalgeb(aalgeb: String) {
    if (arrowalgebs.contains(aalgeb)) arrowalgebs = arrowalgebs.filter(_ != aalgeb) else arrowalgebs = arrowalgebs :+ aalgeb
  }
}

case class Book(
    var positions: Map[String, BookPosition] = Map[String, BookPosition](),
    var essay: String = ""
) {
  def get(fen: String): BookPosition = {
    if (!positions.contains(fen)) positions += (fen -> BookPosition())
    positions(fen)
  }
  def delete(fen: String) {
    if (positions.contains(fen)) positions -= fen
  }
}
