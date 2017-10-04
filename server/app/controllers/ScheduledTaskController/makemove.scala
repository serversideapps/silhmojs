package controllers

import javax.inject.Inject

import akka.actor._

import upickle.default._

import scala.concurrent.Future
import scala.util._

import scala.concurrent.ExecutionContext.Implicits.global

import shared._
import utils.misc.MyTimeUtils._
import smartchess._

import utils.misc._

import shared.SharedSettings._

object makemove_object {
  import tables._

  var g1 = GlickoData()
  var g2 = GlickoData()

  var glickocnt = 0

  var totalscore = 0.0

  def glickotest {

    println(s"----------------------\nGame $glickocnt\n----------------------")
    val result = List(0.5, 1.0)(r.nextInt(2))

    val newg1 = Glicko.calc(g1, g2, result, true)
    val newg2 = Glicko.calc(g2, g1, 1.0 - result, true)

    g1 = newg1
    g2 = newg2

    totalscore += result

    glickocnt += 1

    println(s"----------------------\nTotal score $totalscore vs. ${glickocnt / 2} expected\n----------------------")
    println(s"----------------------\nPlayer 1: ${g1}\n----------------------")
    println(s"----------------------\nPlayer 2: ${g2}\n----------------------")

  }

  def makemove(k: String, setalgeb: String): Tuple2[Boolean, Boolean] =
    {

      var algeb = setalgeb

      //glickotest

      def quit4(t: Table): Boolean = {
        val b = getb(t)
        t.players(b.turni).resigned = true
        val nh = t.nonresignedhumans
        if (nh.length <= 0) {
          b.b4.ResignAll()
          b.b4.Flush
          t.fen = b.report_fen
          terminate(k, b.b4.gameresult, "Adjudication")
          return true
        }
        false
      }

      if (!tables.contains(k)) return (false, false)
      val t = tables(k)
      if (algeb == "resign") {
        if (!t.IS_FOUR_PLAYER) {
          terminate(k, "resign")
          return (true, true)
        } else {
          // resign does not necessarily terminate the game in four player          
          if (quit4(t)) return (true, true)
        }
      }
      if (istimedout(t)) {
        if (!t.IS_FOUR_PLAYER) {
          terminate(k, "timeout")
          return (true, true)
        } else {
          // timeout does not necessarily terminate the game in four player          
          algeb = "resign"
          if (quit4(t)) return (true, true)
        }
      }
      val b = getb(t)
      if (!b.IS_FLICK) {
        if (!b.isAlgebLegal(algeb)) {
          println("error : trying to make illegal move " + algeb)
          return (false, false)
        }
      }
      if (!t.inprogress) {
        val g = new game(t.variant)
        if (!b.IS_FLICK) {
          if (b.IS_FOUR_PLAYER) {
            // TODO
          } else
            g.set_from_fen(b.report_fen)
        }
        val now = new java.util.Date
        g.pgn_headers += ("Date" -> formatDateAsDateOnly(now))
        g.pgn_headers += ("Time" -> formatDateAsTimeOnly(now))
        if (!b.IS_FOUR_PLAYER) {
          g.pgn_headers += ("White" -> t.players(0).handle)
          g.pgn_headers += ("Black" -> t.players(1).handle)
        } else {
          g.pgn_headers += ("White" -> t.players(0).handle)
          g.pgn_headers += ("Black" -> t.players(2).handle)
          g.pgn_headers += ("Yellow" -> t.players(1).handle)
          g.pgn_headers += ("Red" -> t.players(3).handle)
        }
        games += (k -> g)
      }
      updateplayertime(t, b.turni)
      if (!b.IS_FLICK) {
        b.makeAlgebMove(algeb)
      }
      val g = games(k)
      if (!b.IS_FLICK) {
        if (b.IS_FOUR_PLAYER) {
          // TODO
        } else {
          g.makeAlgebMove(algeb)
          t.algebline = g.current_line_algeb
        }
      }
      t.inprogress = true
      if (!b.IS_FLICK) {
        t.fen = b.report_fen
      } else {
        t.fen = algeb
      }
      if (!b.IS_FLICK) {
        if (b.IS_FOUR_PLAYER) {
          if (!b.b4.Flush) {
            t.fen = b.report_fen
            terminate(k, b.b4.gameresult, "Adjudication")
            return (true, true)
          }
          t.fen = b.report_fen // fen may have changed in flush
        }
        val ptm = t.players(b.turni)
        if (b.IS_FOUR_PLAYER) {
          if (ptm.kind == "ai") {
            val ml = b.b4.LegalMoves()
            if (ml.hasmoves) {
              val m = ml.items(r.nextInt(ml.items.length))
              return makemove(k, m.toalgeb)
            } else {
              // should not happen, because game is already flushed   
              println("error : ai has no legal moves after flush")
              return makemove(k, "-")
            }
          }
        } else {
          b.genMoveListInner
          if (b.status > 0) {
            if (g.is_threefold_repetition) {
              terminate(k, "1/2 - 1/2", "Threefold repetition")
              return (true, true)
            }
            if (g.is_fifty_move_rule) {
              terminate(k, "1/2 - 1/2", "Fifty move rule")
              return (true, true)
            }
          } else {
            if (b.status == board.IS_STALEMATE) terminate(k, "1/2 - 1/2", "Stalemate")
            else terminate(k, "mate")
            return (true, true)
          }
          if (ptm.kind == "ai") {
            val m = b.move_list(r.nextInt(b.move_list.length))
            return makemove(k, m.toAlgeb)
          }
        }
        ptm.startedthinkingms = System.currentTimeMillis()
      } else {
        val ptm = t.players(b.turni)
        if (b.flickresult != "none") {
          terminate(k, b.flickresult, "Pieces flicked")
          return (true, true)
        }
        ptm.startedthinkingms = System.currentTimeMillis()
      }
      (true, false)
    }
}