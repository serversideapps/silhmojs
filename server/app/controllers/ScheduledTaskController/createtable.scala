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

object createtable_object {
  import tables._

  def createId: String = java.util.UUID.randomUUID.toString

  def create(
    variant: String = smartchess.board.DEFAULT_VARIANT,
    timecontrol: String = DEFAULT_TIME_CONTROL
  ): Tuple3[Boolean, String, Table] = {
    val id = createId
    val s = System.currentTimeMillis()
    val b = new board(variant)
    b.reset
    val table = Table(
      id,
      variant = variant,
      timecontrol = timecontrol,
      created = "" + s,
      createdF = formatDateAsTime(new java.util.Date(s)),
      inprogress = false,
      fen = b.report_fen
    )
    val t = timeControlToTimeMs(timecontrol)
    for (p <- table.players) {
      p.timems = t
      p.lasttimems = t
    }
    val isdupl = isDuplicate(table)
    if (isdupl) { /*println("duplicate")*/ } else {
      tables += (id -> table)
    }
    (isdupl, id, table)
  }

  def createandsave(
    variant: String = smartchess.board.DEFAULT_VARIANT,
    timecontrol: String = DEFAULT_TIME_CONTROL
  ): Tuple3[Boolean, String, Table] = {
    val ct = create(variant, timecontrol)
    if (!ct._1) tableDAO.set(ct._2, write[Table](ct._3))
    (!ct._1, ct._2, ct._3)
  }

  def delete(k: String) {
    tables -= k
    tableDAO.delete(k)
  }

  def handleidles {
    for ((k, v) <- tables) {
      if (!v.inprogress) {
        tables(k).idle += 1
        if (tables(k).idle >= TABLE_IDLE_PERIOD) {
          println(s"deleting table $k idle for " + tables(k).idle)
          delete(k)
        }
      }
    }
  }

  def isDuplicate(t: Table): Boolean =
    {
      var dupcnt = 0
      for ((k, v) <- tables) if (t.isDuplicateOf(v)) dupcnt += 1
      dupcnt > 3
    }

}