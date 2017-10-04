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

import scala.collection.mutable.ArrayBuffer

object playermanagement_object {
  import tables._

  var changedtables = ArrayBuffer[String]()

  def sitplayer(k: String, i: Int, player: Player): Boolean = {
    changedtables = ArrayBuffer[String]()
    if (!tables.contains(k)) return false
    val t = tables(k)
    if (player.human && (t.hashandle(player.handle))) return false
    for ((sk, st) <- tables if (sk != k)) {
      val useri = st.getuseri(player.handle)
      if ((useri >= 0) && (!player.ai)) {
        if (st.inprogress) return false
        if (!st.terminated) st.players(useri).handle = ""
        changedtables += sk
      }
    }
    t.players(i) = player
    val p = t.players(i)
    p.timems = timeControlToTimeMs(t.timecontrol)
    p.lasttimems = p.timems
    if (!t.hashuman) t.resetplayers
    t.idle = 0
    t.players(i).valid
  }

  def updaterating(uuid: java.util.UUID, g: GlickoData) {
    userDAO.find(uuid).flatMap {
      case Some(user) => // Update user with rating
        userDAO.save(user.copy(
          rating = g.rating,
          rd = g.rd,
          lastrated = g.lastrated
        ))
      case _ => { println("could not update user rating"); Future.successful({}) }
    }
  }

}