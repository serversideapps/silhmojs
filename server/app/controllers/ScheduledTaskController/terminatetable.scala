package controllers

import javax.inject.Inject

import akka.actor._

import upickle.default._

import scala.concurrent.Future
import scala.util._

import scala.concurrent.ExecutionContext.Implicits.global

import shared._
import utils.misc.MyTimeUtils._
import shared.SharedTimeUtils._
import smartchess._

import utils.misc._

import shared.SharedSettings._

object PgnTranslations {

  val international = Map[String, String](
    "ccfg.whiteresigned" -> "White Resigned",
    "ccfg.blackresigned" -> "Black Resigned",
    "ccfg.whiteflagged" -> "White Flagged",
    "ccfg.blackflagged" -> "Black Flagged",
    "ccfg.whitemated" -> "White Mated",
    "ccfg.blackmated" -> "Black Mated"
  )
  val RESULTREASONS = international.keys.toList

  def t(phrase: String): String = {
    if (international.contains(phrase)) return international(phrase)
    phrase
  }

}

object terminatetable_object {
  import tables._
  def terminate(k: String, action: String, setresultreason: String = "") {
    if (tables(k).terminated) return
    terminate_inner(k, action, setresultreason)
    val t = tables(k)
    val g = games(k)
    val b = getb(t)
    if (b.IS_FOUR_PLAYER) b.b4.DetermineGameresult()
    val pgnresult = if (!b.IS_FOUR_PLAYER) t.result.replaceAll(" ", "") else b.b4.gameresult.replaceAll(" ", "")
    g.pgn_headers += ("Result" -> pgnresult)
    g.pgn_headers += ("Termination" -> PgnTranslations.t(t.resultreason))
    if (!b.IS_FOUR_PLAYER) {
      g.pgn_headers += ("WhiteElo" -> t.players(0).formattedrating)
      g.pgn_headers += ("BlackElo" -> t.players(1).formattedrating)
    } else {
      g.pgn_headers += ("WhiteElo" -> t.players(0).formattedrating)
      g.pgn_headers += ("YellowElo" -> t.players(1).formattedrating)
      g.pgn_headers += ("BlackElo" -> t.players(2).formattedrating)
      g.pgn_headers += ("RedElo" -> t.players(3).formattedrating)
    }
    g.pgn_headers += ("Site" -> SITE_URL)
    g.pgn_headers += ("Event" -> "Online chess game")
    g.pgn_headers += ("TimeControl" ->
      ((timeControlToTimeMs(t.timecontrol) / 1000.0).toInt
        + "+" +
        (timeControlToIncrementMs(t.timecontrol) / 1000.0).toInt))
    if (!b.IS_FOUR_PLAYER) {
      val rv = resultvalue(pgnresult)
      val p0 = t.players(0)
      val p1 = t.players(1)
      if (p0.human && (p0.uuid != null) && p1.human && (p1.uuid != null)) {
        val g0 = GlickoData(p0.rating, p0.rd, p0.lastrated)
        val g1 = GlickoData(p1.rating, p1.rd, p1.lastrated)
        val g0new = Glicko.calc(g0, g1, rv)
        val g1new = Glicko.calc(g1, g0, 1.0 - rv)
        g.pgn_headers += ("NewWhiteElo" -> ("" + g0new.rating.toInt))
        g.pgn_headers += ("NewBlackElo" -> ("" + g1new.rating.toInt))
        updaterating(p0.uuid, g0new)
        updaterating(p1.uuid, g1new)
      }
    } else {
      var glickos = Array(
        GlickoData(t.players(0).rating, t.players(0).rd, t.players(0).lastrated),
        GlickoData(t.players(1).rating, t.players(1).rd, t.players(1).lastrated),
        GlickoData(t.players(2).rating, t.players(2).rd, t.players(2).lastrated),
        GlickoData(t.players(3).rating, t.players(3).rd, t.players(3).lastrated)
      )
      for (m <- b.b4.matches) {
        val p0 = t.players(m.p0)
        val p1 = t.players(m.p1)
        if (p0.human && (p0.uuid != null) && p1.human && (p1.uuid != null)) {
          val score = m.score
          val g0 = glickos(m.p0)
          val g1 = glickos(m.p1)
          val g0new = Glicko.calc(g0, g1, score)
          glickos(m.p0) = g0new
        }
      }
      g.pgn_headers += ("NewWhiteElo" -> ("" + glickos(0).rating.toInt))
      g.pgn_headers += ("NewYellowElo" -> ("" + glickos(1).rating.toInt))
      g.pgn_headers += ("NewBlackElo" -> ("" + glickos(2).rating.toInt))
      g.pgn_headers += ("NewRedElo" -> ("" + glickos(3).rating.toInt))
      for (i <- 0 to board4.LAST_PLAYER) {
        updaterating(t.players(i).uuid, glickos(i))
      }
    }
    val nowms = System.currentTimeMillis().toDouble
    val pgn = g.report_pgn
    val White = g.get_header("White")
    val Black = g.get_header("Black")
    val Yellow = g.get_header("Yellow")
    val Red = g.get_header("Red")
    val Result = g.get_header("Result")
    val Date = g.get_header("Date")
    val Time = g.get_header("Time")
    for (i <- 0 to b.numplayers - 1) {
      val id = java.util.UUID.randomUUID.toString()
      gameDAO.set(id, Game(
        usergameid = id,
        createdms = nowms,
        White = White,
        Black = Black,
        Yellow = Yellow,
        Red = Red,
        Result = Result,
        Date = Date,
        Time = Time,
        player0 = t.players(i).handle,
        pgn = pgn,
        variant = t.variant
      ))
    }
  }

  def terminate_inner(k: String, action: String, setresultreason: String = "") {
    var resultreason = setresultreason
    val t = tables(k)
    val i = getturni(t)
    val numplayers = getnumplayers(t)
    var result = "terminated"
    if ((action == "resign") || (action == "timeout")) {
      if (numplayers == 2) {
        if (i == 0) {
          result = "0 - 1"
          resultreason = if (action == "timeout") "ccfg.whiteflagged" else "ccfg.whiteresigned"
        } else {
          resultreason = if (action == "timeout") "ccfg.blackflagged" else "ccfg.blackresigned"
          result = "1 - 0"
        }
      }
    } else if (action == "mate") {
      if (numplayers == 2) {
        if (i == 0) {
          result = "0 - 1"
          resultreason = "ccfg.whitemated"
        } else {
          resultreason = "ccfg.blackmated"
          result = "1 - 0"
        }
      }
    } else {
      result = action
    }
    t.result = result
    t.resultreason = resultreason
    t.terminated = true
    t.inprogress = false
    if (action != "timeout") updateplayertime(t, i)
  }
}