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

object timemanagement_object {
  import tables._

  def updateplayertime(t: Table, i: Int) {
    var usedtime = 0.0
    val p = t.players(i)
    if (p.startedthinkingms > 0.0) {
      usedtime = System.currentTimeMillis() - p.startedthinkingms
    }
    p.lasttimems += timeControlToIncrementMs(t.timecontrol) - usedtime
    p.timems = p.lasttimems
  }

  def istimedout(t: Table): Boolean = {
    var usedtime = 0.0
    val b = getb(t)
    val i = b.turni
    val p = t.players(i)
    if (p.startedthinkingms > 0.0) {
      usedtime = System.currentTimeMillis() - p.startedthinkingms
    }
    val timedout = usedtime > p.timems
    if (timedout) p.timems = 0
    timedout
  }

  def gettimedoutlist: List[String] = (for ((k, v) <- tables if ((!v.terminated) && istimedout(v))) yield k).toList

  def terminatetimedoutlist(ks: List[String]) {
    for (k <- ks) {
      val t = tables(k)
      if (t.IS_FOUR_PLAYER) {
        makemove(k, "resign")
      } else {
        terminate(k, "timeout")
      }
    }
  }

}