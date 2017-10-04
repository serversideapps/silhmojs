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

object tableutils_object {
  import tables._
  def gettable(k: String, updatetime: Boolean = true, handle: String = ""): Tuple2[String, Table] = {
    val (truek, table) = gettable_inner(k, handle)
    if ((truek != null) && (table != null) && (updatetime)) {
      val b = getb(table)
      val p = table.players(b.turni)
      if ((table.inprogress) && (p.startedthinkingms > 0.0)) {
        val current = System.currentTimeMillis
        p.lasttimems = p.lasttimems + p.startedthinkingms - current
        p.timems = p.lasttimems
        p.startedthinkingms = current
      }
    }
    (truek, table)
  }

  def usersongoinggame(handle: String): Option[String] = {
    if (handle == "") return None
    for ((k, t) <- tables) {
      if (t.inprogress && t.humanhandles.contains(handle)) {
        return Some(k)
      }
    }
    None
  }

  def gettable_inner(k: String, handle: String = ""): Tuple2[String, Table] =
    {
      if (tables.size == 0) createandsave()
      if (k == "default") {
        val uogopt = usersongoinggame(handle)
        if (!uogopt.isEmpty) {
          val uog = uogopt.get
          return (uog, tables(uog))
        } else {
          val k = tablekeyssorted(0)
          return (k, tables(k))
        }
      }
      if (!tables.contains(k)) return (null, null)
      (k, tables(k))
    }

  def getb(t: Table): board = {
    val b = new board(t.variant)
    b.set_from_fen(t.fen)
    b
  }

  def getturni(t: Table): Int = getb(t).turni

  def getnumplayers(t: Table): Int = getb(t).numplayers

  def resultvalue(pgnresult: String): Double = pgnresult match {
    case "1-0" => 1.0
    case "0-1" => 0.0
    case _ => 0.5
  }

  def sorttablekeysfunc(keya: String, keyb: String): Boolean = {
    val a = tables(keya)
    val b = tables(keyb)
    SharedLogic.sorttablekeyslogic(a, b)
  }

}