package smartchess

case class Match4(
    p0: Int,
    p1: Int,
    score: Double
) {
}

object PlacedPiece {
  def ppfa(algeb: String, p: piece4) = PlacedPiece(square4.square4fromalgeb(algeb), p)
}

case class PlacedPiece(
    var sq: square4,
    var p: piece4
) {
}

case class MoveList(
    val items: scala.collection.mutable.ArrayBuffer[move4] = scala.collection.mutable.ArrayBuffer[move4]()
) {
  import move4._
  def +=(m: move4) {
    if (m == NO_MOVE) return
    items += m.cclone
  }
  def append(ml: MoveList) {
    for (m <- ml.items) this += m.cclone
  }
  def hasmove(m4: move4): Boolean = {
    for (item <- items) if (item.isRoughlyEqualTo(m4)) return true
    false
  }

  def hasmoves = items.length > 0
}

object PieceVector4 {
  val ROOK_N = PieceVector4(0, -1)
  val ROOK_S = PieceVector4(0, 1)
  val ROOK_W = PieceVector4(-1, 0)
  val ROOK_E = PieceVector4(1, 0)

  val BISHOP_NW = PieceVector4(-1, -1)
  val BISHOP_SW = PieceVector4(1, -1)
  val BISHOP_NE = PieceVector4(-1, 1)
  val BISHOP_SE = PieceVector4(1, 1)

  val KNIGHT_NWN = PieceVector4(-1, -2)
  val KNIGHT_NWS = PieceVector4(-2, -1)
  val KNIGHT_SWN = PieceVector4(-2, 1)
  val KNIGHT_SWS = PieceVector4(-1, 2)
  val KNIGHT_NEN = PieceVector4(1, -2)
  val KNIGHT_NES = PieceVector4(2, -1)
  val KNIGHT_SEN = PieceVector4(2, 1)
  val KNIGHT_SES = PieceVector4(1, 2)

  val ROOK_VECTS = List(ROOK_N, ROOK_S, ROOK_W, ROOK_E)
  val BISHOP_VECTS = List(BISHOP_NW, BISHOP_SW, BISHOP_NE, BISHOP_SE)
  val QUEEN_VECTS = ROOK_VECTS ::: BISHOP_VECTS
  val KNIGHT_VECTS = List(KNIGHT_NWN, KNIGHT_NWS, KNIGHT_SWN, KNIGHT_SWS, KNIGHT_NEN, KNIGHT_NES, KNIGHT_SEN, KNIGHT_SES)

  val ALL_PIECES = List("p", "n", "b", "r", "q", "k")

  val ALL_NON_PAWN_PIECES = List("n", "b", "r", "q", "k")

  val MOVE_GEN_VECTORS = Map[String, List[PieceVector4]](
    "r" -> ROOK_VECTS,
    "b" -> BISHOP_VECTS,
    "q" -> QUEEN_VECTS,
    "n" -> KNIGHT_VECTS,
    "k" -> QUEEN_VECTS
  )

  val PAWN_DIRS = Map[Int, PieceVector4](
    0 -> PieceVector4(0, -1),
    1 -> PieceVector4(1, 0),
    2 -> PieceVector4(0, 1),
    3 -> PieceVector4(-1, 0)
  )

  val PAWN_CAPTURES_LEFT = Map[Int, PieceVector4](
    0 -> PieceVector4(-1, -1),
    1 -> PieceVector4(1, -1),
    2 -> PieceVector4(1, 1),
    3 -> PieceVector4(-1, 1)
  )

  val PAWN_CAPTURES_RIGHT = Map[Int, PieceVector4](
    0 -> PieceVector4(1, -1),
    1 -> PieceVector4(1, 1),
    2 -> PieceVector4(-1, 1),
    3 -> PieceVector4(-1, -1)
  )
}

object move4 {
  import square4._
  import piece4._
  val NO_MOVE = move4(NO_SQUARE, NO_SQUARE, NO_PIECE)

  def move4fromalgeb(algeb: String): move4 = {
    val fileparts = algeb.split("[0-9]+").toList
    val rankparts = algeb.split("[a-z]+").toList
    if ((fileparts.length < 2) || (fileparts.length > 3) || (rankparts.length != 3)) return NO_MOVE
    val algeb0 = fileparts(0) + rankparts(1)
    val algeb1 = fileparts(1) + rankparts(2)
    val from = square4fromalgeb(algeb0)
    val to = square4fromalgeb(algeb1)
    val prompiece = if (fileparts.length > 2) piece4(fileparts(2).substring(0, 1), 0) else NO_PIECE
    move4(from, to, prompiece)
  }
}

case class move4(
    var from: square4,
    var to: square4,
    var prompiece: piece4 = piece4.NO_PIECE,
    var capture: Boolean = false
) {
  import square4._
  import piece4._
  import move4._

  check // check upon construction

  def cclone: move4 = move4(from.cclone, to.cclone, prompiece.cclone, capture)

  def isRoughlyEqualTo(compm: move4): Boolean = {
    if (from.isEqualTo(compm.from) && to.isEqualTo(compm.to)) return true
    false
  }

  def check: Boolean = {
    if ((from == NO_SQUARE) || (to == NO_SQUARE)) {
      from = NO_SQUARE
      to = NO_SQUARE
      prompiece = NO_PIECE
      return false
    }
    true
  }

  def toalgeb: String = {
    if (this == NO_MOVE) return ("-")
    from.toalgeb + to.toalgeb + (if (ispromotion) prompiece.kind else "")
  }

  def ispromotion = (prompiece != NO_PIECE)
}

case class PieceVector4(
    dfile: Int,
    drank: Int
) {
}

object square4 {
  val EMPTY_PART = 3
  val BOARD_SIZE4 = 14
  val EMPTY_LOWER_LAST = EMPTY_PART - 1
  val EMPTY_UPPER_FIRST = BOARD_SIZE4 - EMPTY_PART
  val NO_FILE = -1
  val NO_RANK = -1
  val NO_SQUARE = square4(NO_FILE, NO_RANK)
  val LAST_FILE = BOARD_SIZE4 - 1
  val LAST_RANK = BOARD_SIZE4 - 1
  val FILE_TO_ALGEB = Map(0 -> "a", 1 -> "b", 2 -> "c", 3 -> "d", 4 -> "e", 5 -> "f", 6 -> "g", 7 -> "h", 8 -> "i", 9 -> "j", 10 -> "k", 11 -> "l", 12 -> "m", 13 -> "n", 14 -> "o")
  val ALGEB_TO_FILE = Map("a" -> 0, "b" -> 1, "c" -> 2, "d" -> 3, "e" -> 4, "f" -> 5, "g" -> 6, "h" -> 7, "i" -> 8, "j" -> 9, "k" -> 10, "l" -> 11, "m" -> 12, "n" -> 13, "o" -> 14)
  def filetoalgeb(file: Int): String = {
    if ((file < 0) || (file >= BOARD_SIZE4)) return "-"
    FILE_TO_ALGEB(file)
  }
  def algebtofile(algeb: String): Int = {
    if (!ALGEB_TO_FILE.contains(algeb)) return NO_FILE
    ALGEB_TO_FILE(algeb)
  }
  def algebtorank(algeb: String): Int = {
    var r = NO_RANK
    try {
      r = LAST_RANK - (algeb.toInt - 1)
      if ((r < 0) || (r >= BOARD_SIZE4)) return NO_RANK
    } catch { case e: Throwable => return NO_RANK }
    r
  }
  def square4fromalgeb(algeb: String): square4 = {
    if ((algeb.length < 2) || (algeb.length > 3)) return NO_SQUARE
    val f = algebtofile(algeb.substring(0, 1))
    if (f == NO_FILE) return NO_SQUARE
    val r = algebtorank(algeb.substring(1))
    if (r == NO_RANK) return NO_SQUARE
    square4(f, r)
  }
}

case class square4(
    var file: Int,
    var rank: Int
) {
  import square4._
  import board4._

  check // check after construction

  def isEqualTo(compsq: square4): Boolean = {
    if ((file == compsq.file) && (rank == compsq.rank)) return true
    false
  }

  def check: Boolean = {
    if (isvalid) return true
    file = NO_FILE
    rank = NO_RANK
    return false
  }

  def cclone: square4 = square4(file, rank)
  def +(pv: PieceVector4): square4 = square4(file + pv.dfile, rank + pv.drank)
  def -(pv: PieceVector4): square4 = square4(file - pv.dfile, rank - pv.drank)
  def +=(pv: PieceVector4): Boolean = {
    file += pv.dfile
    rank += pv.drank
    check
  }
  def toalgeb = filetoalgeb(file) + ((LAST_RANK - rank) + 1)

  def rot(c: Int): square4 = {
    if ((c == 1) || (c == -3)) return square4(LAST_RANK - rank, file)
    if ((c == 2) || (c == -2)) return square4(LAST_FILE - file, LAST_RANK - rank)
    if ((c == 3) || (c == -1)) return square4(rank, LAST_FILE - file)
    square4(file, rank)
  }

  def isempty: Boolean = (
    (file <= EMPTY_LOWER_LAST) && (rank <= EMPTY_LOWER_LAST) ||
    (file <= EMPTY_LOWER_LAST) && (rank >= EMPTY_UPPER_FIRST) ||
    (file >= EMPTY_UPPER_FIRST) && (rank <= EMPTY_LOWER_LAST) ||
    (file >= EMPTY_UPPER_FIRST) && (rank >= EMPTY_UPPER_FIRST)
  )

  def isvalid: Boolean = ((!isempty) && (file >= 0) && (file <= LAST_FILE) && (rank >= 0) && (rank <= LAST_RANK))
}

object piece4 {
  val PIECES = List("p", "n", "b", "r", "q", "k")
  val PROM_PIECES = List("q", "r", "b", "n")
  val PROM_PIECE_CHARS = List('q', 'r', 'b', 'n')
  val NO_PIECE = piece4("-", 0)

  def piece4fromalgeb(algeb: String): piece4 = {
    if (algeb.length != 2) return NO_PIECE
    val pl = algeb.substring(0, 1)
    if (!PIECES.contains(pl)) return NO_PIECE
    val cl = algeb.substring(1)
    var c = -1
    try {
      c = cl.toInt
    } catch { case e: Throwable => return NO_PIECE }
    if ((c < 0) || (c >= board4.NUM_PLAYERS)) return NO_PIECE
    piece4(pl, c)
  }
}

case class piece4(
    kind: String = "-",
    color: Int = 0
) {
  def cclone: piece4 = piece4(kind, color)

  def toalgeb: String = kind + color

  def isEqualTo(compp: piece4): Boolean = {
    if ((kind == compp.kind) && (color == compp.color)) return true
    false
  }
}

object castlingRight4 {
  val CASTLING_RIGHT4S = List("kq", "k", "q", "-")
  val NO_CASTLING_RIGHT = castlingRight4("")
  def castlingRight4fromalgeb(algeb: String): castlingRight4 = {
    if (CASTLING_RIGHT4S.contains(algeb)) return castlingRight4(algeb)
    NO_CASTLING_RIGHT
  }
}

case class castlingRight4(
    var castlingright4: String = "-"
) {
  def toalgeb: String = castlingright4

  def has(right: String): Boolean = castlingright4.contains(right)

  def disable(right: String) {
    if (has(right)) {
      castlingright4 = castlingright4.replaceAll(right, "")
      if (castlingright4 == "") castlingright4 = "-"
    }
  }
}

object castlingRights4 {
  import board4._
  import castlingRight4._
  import PlacedPiece._
  import piece4._

  def NO_CASTLING_RIGHTS4 = castlingRights4(Array())

  def castlingRights4fromalgeb(algeb: String): castlingRights4 = {
    val parts = algeb.split(" ")
    if (parts.length != NUM_PLAYERS) return NO_CASTLING_RIGHTS4
    val crs = castlingRights4()
    for (i <- 0 to LAST_PLAYER) {
      val cr = castlingRight4fromalgeb(parts(i))
      if (cr == NO_CASTLING_RIGHTS4) return NO_CASTLING_RIGHTS4
      crs.castlingrights4(i) = cr
    }
    crs
  }

  val CASTLING_DISABLE_SQUARES = Map[String, Tuple2[Int, String]](
    "d1" -> (0, "q"),
    "h1" -> (0, "kq"),
    "k1" -> (0, "k"),
    "a11" -> (1, "q"),
    "a7" -> (1, "kq"),
    "a4" -> (1, "k"),
    "k14" -> (2, "q"),
    "g14" -> (2, "kq"),
    "d14" -> (2, "k"),
    "n4" -> (3, "q"),
    "n8" -> (3, "kq"),
    "n11" -> (3, "k")
  )

  val CALGEB_0_k =
    List(ppfa("h1", NO_PIECE), ppfa("j1", piece4("k", 0)), ppfa("k1", NO_PIECE), ppfa("i1", piece4("r", 0)))
  val CALGEB_0_q =
    List(ppfa("h1", NO_PIECE), ppfa("f1", piece4("k", 0)), ppfa("d1", NO_PIECE), ppfa("g1", piece4("r", 0)), ppfa("e1", NO_PIECE))
  val CALGEB_1_k =
    List(ppfa("a7", NO_PIECE), ppfa("a5", piece4("k", 1)), ppfa("a4", NO_PIECE), ppfa("a6", piece4("r", 1)))
  val CALGEB_1_q =
    List(ppfa("a7", NO_PIECE), ppfa("a9", piece4("k", 1)), ppfa("a11", NO_PIECE), ppfa("a8", piece4("r", 1)), ppfa("a10", NO_PIECE))
  val CALGEB_2_k =
    List(ppfa("g14", NO_PIECE), ppfa("e14", piece4("k", 2)), ppfa("d14", NO_PIECE), ppfa("f14", piece4("r", 2)))
  val CALGEB_2_q =
    List(ppfa("g14", NO_PIECE), ppfa("i14", piece4("k", 2)), ppfa("k14", NO_PIECE), ppfa("h14", piece4("r", 2)), ppfa("j14", NO_PIECE))
  val CALGEB_3_k =
    List(ppfa("n8", NO_PIECE), ppfa("n10", piece4("k", 3)), ppfa("n11", NO_PIECE), ppfa("n9", piece4("r", 3)))
  val CALGEB_3_q =
    List(ppfa("n8", NO_PIECE), ppfa("n6", piece4("k", 3)), ppfa("n4", NO_PIECE), ppfa("n5", piece4("r", 3)), ppfa("n7", NO_PIECE))

  val CASTLE_TEST_EMTPY = Map[String, List[Int]](
    "k" -> List(1, 3),
    "q" -> List(1, 3, 4)
  )

  val CASTLE_TEST_ATTACK = List[Int](0, 1, 3)

  val CASTLING_ALGEBS = Map[String, List[PlacedPiece]](
    "h1j1" -> CALGEB_0_k,
    "h1k1" -> CALGEB_0_k,
    "h1f1" -> CALGEB_0_q,
    "h1d1" -> CALGEB_0_q,
    "a7a5" -> CALGEB_1_k,
    "a7a4" -> CALGEB_1_k,
    "a7a9" -> CALGEB_1_q,
    "a7a11" -> CALGEB_1_q,
    "g14e14" -> CALGEB_2_k,
    "g14d14" -> CALGEB_2_k,
    "g14i14" -> CALGEB_2_q,
    "g14k14" -> CALGEB_2_q,
    "n8n10" -> CALGEB_3_k,
    "n8n11" -> CALGEB_3_k,
    "n8n6" -> CALGEB_3_q,
    "n8n4" -> CALGEB_3_q
  )
}

case class castlingRights4(
    var castlingrights4: Array[castlingRight4] = Array[castlingRight4](castlingRight4(), castlingRight4(), castlingRight4(), castlingRight4())
) {
  def toalgeb = (for (cr4 <- castlingrights4) yield cr4.toalgeb).toList.mkString(" ")

  def disable(color: Int, right: String) {
    val cr = castlingrights4(color)
    if (right.contains("k")) {
      cr.disable("k")
    }
    if (right.contains("q")) {
      cr.disable("q")
    }
  }
}

object board4Rep {
  import square4._
  import piece4._
  val NO_BOARD4REP = board4Rep(Map(NO_SQUARE -> NO_PIECE))
  def board4Repfromalgeb(algeb: String): board4Rep = {
    val br = board4Rep()
    br.reset
    val mainparts = algeb.split("  ")
    for (algebpart <- mainparts) {
      val parts = algebpart.split(" ")
      if (parts.length != 2) return NO_BOARD4REP
      val sq4 = square4fromalgeb(parts(0))
      if (sq4 == NO_SQUARE) return NO_BOARD4REP
      val p4 = piece4fromalgeb(parts(1))
      if (p4 != NO_PIECE) br.put(sq4, p4)
    }
    br
  }
}

case class board4Rep(
    var rep4: Map[square4, piece4] = Map[square4, piece4]()
) {
  import board4._
  import square4._
  import piece4._
  def toalgeb = (for ((sq4, p4) <- rep4) yield sq4.toalgeb + " " + p4.toalgeb).mkString("  ")

  def put(sq4: square4, p4: piece4) {
    rep4 += (sq4 -> p4)
  }

  def get(sq4: square4): piece4 = {
    if (!rep4.contains(sq4)) return NO_PIECE
    rep4(sq4)
  }

  def set(sq4: square4, p4: piece4) {
    rep4 += (sq4 -> p4)
  }

  def reset = {
    rep4 = Map[square4, piece4]()
  }

  def startpos {
    reset
    val baseline4 = List("r", "n", "b", "k", "q", "b", "n", "r")
    for (i <- 0 to (baseline4.length - 1)) {
      val basesq4 = square4(i + EMPTY_PART, 0).rot(2)
      val pawnsq4 = square4(i + EMPTY_PART, 1).rot(2)
      for (c <- 0 to LAST_PLAYER) {
        put(basesq4.rot(c), piece4(baseline4(i), c))
        put(pawnsq4.rot(c), piece4("p", c))
      }
    }
  }

  def reportPrintable: String = {
    var buff = ""
    for (r <- 0 to LAST_RANK) {
      for (f <- 0 to LAST_FILE) {
        buff += get(square4(f, r)).toalgeb + " "
      }
      buff += "\n"
    }
    buff
  }
}

object board4 {
  val NUM_PLAYERS = 4
  val LAST_PLAYER = NUM_PLAYERS - 1

  val FIFTY_MOVE_RULE = 100

  def init {
  }
}

case class board4() {
  import board4._
  import castlingRight4._
  import castlingRights4._
  import board4Rep._
  import piece4._
  import PieceVector4._
  import square4._
  import move4._

  //////////////////////////////////////////////////////////////

  var rep4 = board4Rep()

  var turn4: Int = 0

  var castlingrights4 = castlingRights4()

  var incturn_count: Int = 0

  var termination_counts: Array[Int] = Array(-1, -1, -1, -1)

  var halfmove_clock4: Int = 0

  var gameresult: String = "-"

  var lastalgeb: String = "-"

  //////////////////////////////////////////////////////////////

  reset4

  def report_fen4: String = {
    rep4.toalgeb + "   " +
      turn4 + "   " +
      castlingrights4.toalgeb + "   " +
      incturn_count + "   " +
      ((for (tc <- termination_counts) yield "" + tc).mkString(" ")) + "   " +
      halfmove_clock4 + "   " +
      gameresult + "   " +
      lastalgeb
  }

  def reset4hard {
    rep4 = board4Rep()
    turn4 = 0
    castlingrights4 = castlingRights4()
    incturn_count = 0
    termination_counts = Array(-1, -1, -1, -1)
    halfmove_clock4 = 0
    gameresult = "-"
    lastalgeb = "-"
  }

  def reset4 {
    reset4hard
    rep4.startpos
    val fr = castlingRight4("kq")
    castlingrights4 = castlingRights4(Array(fr, fr, fr, fr))
  }

  def set_from_fen4(fen: String): Boolean = {

    val parts = fen.split("   ").toList

    if (parts.length < 8) return false

    //////////////////////////////////////////////////

    val r4 = board4Repfromalgeb(parts(0))
    if (r4 == NO_BOARD4REP) return false

    //////////////////////////////////////////////////

    var t4 = -1
    try {
      t4 = parts(1).toInt
    } catch { case e: Throwable => return false }
    if ((t4 < 0) || (t4 >= NUM_PLAYERS)) return false

    //////////////////////////////////////////////////

    val crs4 = castlingRights4fromalgeb(parts(2))
    if (crs4 == NO_CASTLING_RIGHTS4) return false

    //////////////////////////////////////////////////

    var itc = -1
    try {
      itc = parts(3).toInt
    } catch { case e: Throwable => return false }
    if (itc < 0) return false

    //////////////////////////////////////////////////

    val tcparts = parts(4).split(" ")

    var i = 0
    var tcd = Array[Int](-1, -1, -1, -1)
    for (tcpart <- tcparts) {
      var tc = -2
      try {
        tc = tcpart.toInt
      } catch { case e: Throwable => return false }
      if (tc < -1) return false
      tcd(i) = tc
      i += 1
    }

    //////////////////////////////////////////////////

    var hmc = -1
    try {
      hmc = parts(5).toInt
    } catch { case e: Throwable => return false }
    if (hmc < 0) return false

    //////////////////////////////////////////////////

    rep4 = r4
    turn4 = t4
    castlingrights4 = crs4
    incturn_count = itc
    termination_counts = tcd
    halfmove_clock4 = hmc
    gameresult = parts(6)
    lastalgeb = parts(7)

    true
  }

  def reportPrintable = rep4.reportPrintable + "\n" + turn4 + " " + castlingrights4.toalgeb

  def MovesForPiece(
    origsq4: square4,
    origp4: piece4,
    withcapture: Boolean = true,
    withnormal: Boolean = true,
    shouldcapturepiecekind: String = "-"
  ): MoveList = {

    if (origp4 == NO_PIECE) return MoveList()

    val origcolor = origp4.color
    val origkind = origp4.kind
    val ml = MoveList()
    if (origkind == "p") {
      val pd = PAWN_DIRS(origcolor)
      val captures = List(PAWN_CAPTURES_LEFT(origcolor), PAWN_CAPTURES_RIGHT(origcolor))
      for (capture <- captures) {
        val sq41 = origsq4 + capture
        val targetp41 = rep4.get(sq41)
        val sq42 = sq41 + pd
        if ((targetp41 != NO_PIECE) && (targetp41.color != origcolor)) {
          if (sq42 == NO_SQUARE) {
            // capture promotion
            for (pp <- PROM_PIECES) {
              val mp = move4(origsq4.cclone, sq41, capture = true, prompiece = piece4(pp, origcolor))
              if (withcapture) {
                if ((shouldcapturepiecekind == "-") || (targetp41.kind == shouldcapturepiecekind)) ml += mp
              }
            }
          } else {
            val m = move4(origsq4.cclone, sq41, capture = true)
            if (withcapture) {
              if ((shouldcapturepiecekind == "-") || (targetp41.kind == shouldcapturepiecekind)) ml += m
            }
          }
        }
      }
      val sq41 = origsq4 + pd
      val targetp41 = rep4.get(sq41)
      if (targetp41 == NO_PIECE) {
        val sq42 = sq41 + pd
        val targetp42 = rep4.get(sq42)

        if (sq42 == NO_SQUARE) {
          // promotion
          for (pp <- PROM_PIECES) {
            val mp = move4(origsq4.cclone, sq41, prompiece = piece4(pp, origcolor))
            if (withnormal) ml += mp
          }
        } else {
          val m1 = move4(origsq4.cclone, sq41)
          if (withnormal) ml += m1
        }

        val backtwo = origsq4 - pd - pd
        if ((targetp42 == NO_PIECE) && (backtwo == NO_SQUARE)) {
          val m2 = move4(origsq4.cclone, sq42)
          if (withnormal) ml += m2
        }
      }
    } else {
      val mgvl = MOVE_GEN_VECTORS(origkind)
      for (mgv <- mgvl) {
        val sq4 = origsq4.cclone
        var ready = false
        while ((sq4 += mgv) && (!ready)) {
          val targetp4 = rep4.get(sq4)
          if (targetp4 == NO_PIECE) {
            val m = move4(origsq4.cclone, sq4.cclone, capture = false)
            if (withnormal) ml += m
          } else {
            if (targetp4.color != origcolor) {
              val m = move4(origsq4.cclone, sq4.cclone, capture = true)
              if (withcapture) {
                if ((shouldcapturepiecekind == "-") || (targetp4.kind == shouldcapturepiecekind)) ml += m
              }
            }
            ready = true
          }
          if ((origkind == "n") || (origkind == "k")) ready = true
        }
      }
    }

    ml

  }

  def FindPiece(kind: String, color: Int = turn4): List[PlacedPiece] =
    (for ((sq, p) <- rep4.rep4 if ((p.kind == kind) && (p.color == color))) yield PlacedPiece(sq, p)).toList

  def WhereIsKing(color: Int = turn4): Option[PlacedPiece] = {
    val kings = FindPiece("k", color)
    if (kings.length <= 0) return None
    Some(kings(0))
  }

  def IsKingCaptured(color: Int): Boolean = WhereIsKing(color).isEmpty

  def IsTerminated(color: Int): Boolean = {
    if (termination_counts(color) >= 0) return true
    if (IsKingCaptured(color)) {
      termination_counts(color) = incturn_count
      return true
    }
    false
  }

  def IsGameTerminated: Boolean = {
    gameresult != "-"
  }

  var matches = scala.collection.mutable.ArrayBuffer[Match4]()

  def MatchScore(pi: Int, oi: Int): Double = {
    val tcpi = termination_counts(pi)
    val tcoi = termination_counts(oi)
    if (!IsTerminated(pi)) {
      if (IsTerminated(oi)) return 1.0
      return 0.5
    } else {
      if (!IsTerminated(oi)) return 0
      if (tcpi > tcoi) return 1.0
      if (tcpi < tcoi) return 0.0
    }
    0.5
  }

  def DetermineGameresult() {
    matches = scala.collection.mutable.ArrayBuffer[Match4]()
    for (playeri <- 0 to LAST_PLAYER) for (opponenti <- 0 to LAST_PLAYER) {
      if (playeri != opponenti) {
        matches += Match4(playeri, opponenti, MatchScore(playeri, opponenti))
      }
    }
    var playerscores = Array[Double](0.0, 0.0, 0.0, 0.0)

    for (m <- matches) playerscores(m.p0) = playerscores(m.p0) + m.score

    gameresult = "%.1f - %.1f - %.1f - %.1f".format(playerscores(0), playerscores(1), playerscores(2), playerscores(3))
  }

  def NumTerminated: Int = {
    var nt = 0
    for (i <- 0 to LAST_PLAYER) {
      if (IsTerminated(i)) nt += 1
    }
    nt
  }

  def HasMoves: Boolean = {
    val ml = LegalMoves()
    ml.hasmoves
  }

  def Flush: Boolean = {
    if (gameresult != "-") return false
    if (NumTerminated >= LAST_PLAYER) {
      // terminated, determine result
      DetermineGameresult()
      return false
    }
    if (halfmove_clock4 > FIFTY_MOVE_RULE) {
      // terminated, determine result
      DetermineGameresult()
      return false
    }
    for (i <- 1 to LAST_PLAYER) {
      if (IsTerminated(turn4)) {
        incTurn()
      } else {
        if (HasMoves) {
          return true
        } else {
          IsTerminated(turn4)
          incTurn()
        }
      }
    }
    if (NumTerminated >= LAST_PLAYER) {
      // terminated, determine result
      DetermineGameresult()
      return false
    }
    true
  }

  def PieceAttacks(kind: String, attackedsq: square4, attackedcolor: Int = turn4) =
    MovesForPiece(
      attackedsq,
      piece4(kind, attackedcolor),
      withcapture = true,
      withnormal = false,
      shouldcapturepiecekind = kind
    )

  def PawnAttacks(attackedsq: square4, attackedcolor: Int = turn4) = {
    val ml = MoveList()
    for (color <- 0 to LAST_PLAYER) {
      if (color != attackedcolor) {
        val captures = List(PAWN_CAPTURES_LEFT(color), PAWN_CAPTURES_RIGHT(color))
        for (capture <- captures) {
          val captureinverse = PieceVector4(-capture.dfile, -capture.drank)
          val testsq = attackedsq + captureinverse
          if (testsq != NO_SQUARE) {
            val testpopt = rep4.rep4.get(testsq)
            if (!testpopt.isEmpty) {
              val testp = testpopt.get
              if ((testp.kind == "p") && (testp.color == color)) {
                val m = move4(testsq, attackedsq)
                ml += m
              }
            }
          }
        }
      }
    }
    ml
  }

  def Attacks(attackedsq: square4, attackedcolor: Int = turn4): MoveList = {
    val ml = MoveList()
    for (kind <- ALL_NON_PAWN_PIECES) ml.append(PieceAttacks(kind, attackedsq, attackedcolor))
    ml.append(PawnAttacks(attackedsq, attackedcolor))
    ml
  }

  def IsSquareInCheck(attackedsq: square4, attackedcolor: Int = turn4): Boolean = {
    val attacks = Attacks(attackedsq, attackedcolor)
    attacks.hasmoves
  }

  def IsInCheck(attackedcolor: Int = turn4): Boolean = {
    val wk = WhereIsKing(attackedcolor)
    if (wk.isEmpty) return true
    IsSquareInCheck(wk.get.sq, attackedcolor)
  }

  def PseudoLegalMoves(color: Int): MoveList = {
    val ml = MoveList()
    for ((sq4, p4) <- rep4.rep4) {
      if (p4 != NO_PIECE) {
        if (p4.color == color) {
          val pml = MovesForPiece(sq4, p4)
          ml.append(pml)
        }
      }
    }
    ml
  }

  def LegalMoves(color: Int = turn4): MoveList = {
    val pseudolegals = PseudoLegalMoves(color)
    val legals = MoveList()
    for (pm <- pseudolegals.items) {
      val test = board4()
      test.set_from_fen4(report_fen4)
      val origturn = turn4
      test.makeMove(pm)
      if (!test.IsInCheck(origturn)) legals += pm
    }
    legals
  }

  def isMoveLegal(m: move4): Boolean = {
    val ml = LegalMoves(turn4)
    if (ml.hasmove(m)) return true
    false
  }

  def isAlgebLegal(algeb: String): Boolean = {
    if (CASTLING_ALGEBS.contains(algeb)) {
      val cm = move4fromalgeb(algeb)
      val frompiece = rep4.get(cm.from)
      if ((frompiece.kind == "k") && (frompiece.color == turn4)) {
        val ca = CASTLING_ALGEBS(algeb)
        val rd = ca(2) // rook disable square
        val cd = CASTLING_DISABLE_SQUARES(rd.sq.toalgeb)
        val castlingcolor = cd._1
        val castlingside = cd._2
        if (castlingcolor != turn4) {
          println("inconsistent castling color in move " + algeb)
          return false
        }
        if (!castlingrights4.castlingrights4(turn4).has(castlingside)) return false
        for (i <- CASTLE_TEST_EMTPY(castlingside)) {
          val testsq = ca(i).sq
          if (rep4.get(testsq) != NO_PIECE) return false
        }
        for (i <- CASTLE_TEST_ATTACK) {
          val testsq = ca(i).sq
          if (IsSquareInCheck(testsq)) return false
        }
        return true
      }
    }
    if (algeb == "-") return true
    if (algeb == "resign") return true
    val m = move4fromalgeb(algeb)
    if (m == NO_MOVE) return false
    val result = isMoveLegal(m)
    result
  }

  def incTurn() {
    turn4 += 1
    if (turn4 > LAST_PLAYER) turn4 = 0

    incturn_count += 1
  }

  def disableCastlingForSquare(dsq: square4) {
    val dsqalgeb = dsq.toalgeb
    if (!CASTLING_DISABLE_SQUARES.contains(dsqalgeb)) return
    val dd = CASTLING_DISABLE_SQUARES(dsqalgeb)
    val color = dd._1
    val right = dd._2
    castlingrights4.disable(color, right)
  }

  def makeMove(m: move4): Boolean = {
    val origsq = m.from
    val origpiece = rep4.get(origsq)
    val targetsq = m.to
    val targetpiece = rep4.get(targetsq)

    disableCastlingForSquare(origsq)
    disableCastlingForSquare(targetsq)

    if ((origpiece.kind == "p") || (targetpiece != NO_PIECE)) halfmove_clock4 = 0 else halfmove_clock4 += 1
    rep4.set(origsq, NO_PIECE)
    rep4.set(targetsq, origpiece)
    if (m.prompiece != NO_PIECE) {
      val trueprompiece = piece4(m.prompiece.kind, turn4)
      rep4.set(targetsq, trueprompiece)
    }
    incTurn()
    true
  }

  def Resign() {
    if (!IsTerminated(turn4)) {
      val wk = WhereIsKing()
      if (!wk.isEmpty) rep4.put(wk.get.sq, NO_PIECE)
      termination_counts(turn4) = incturn_count
    }
    incTurn()
  }

  def ResignAll() {
    for (i <- 0 to LAST_PLAYER) Resign()
  }

  def makeAlgebMove(algeb: String): Boolean = {
    if (CASTLING_ALGEBS.contains(algeb)) {
      val cm = move4fromalgeb(algeb)
      if (rep4.get(cm.from).kind == "k") {
        val ca = CASTLING_ALGEBS(algeb)
        for (plp <- ca) {
          rep4.put(plp.sq, plp.p)
          disableCastlingForSquare(plp.sq)
        }
        incTurn()
        lastalgeb = algeb
        return true
      }
    }
    if (algeb == "resign") {
      Resign()
      return true
    }
    if (algeb == "-") {
      // null move
      incTurn()
      return true
    }
    val m = move4fromalgeb(algeb)
    if (m == NO_MOVE) return false
    lastalgeb = algeb
    makeMove(m)
  }

  def isAlgebPromotion(algeb: String): Boolean = {
    val m = move4fromalgeb(algeb)
    if (m == NO_MOVE) return false
    val origpiece = rep4.get(m.from)
    if ((origpiece.kind != "p") || (origpiece.color != turn4)) return false
    val test = m.to + PAWN_DIRS(turn4)
    test == NO_SQUARE
  }
}