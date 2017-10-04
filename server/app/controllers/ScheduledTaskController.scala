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

import play.api.i18n.{ I18nSupport, Messages, MessagesApi }

case class GarbageCollectGamesForUserMessage(handle: String, maxgames: Int = MAX_STORED_GAMES_PER_USER)

case class GarbageCollector(gameDAO: models.daos.GameDAO) extends Actor {
  def receive = {
    case GarbageCollectGamesForUserMessage(handle, maxgames) => {
      gameDAO.find(handle).flatMap {
        gamelist =>
          Future.successful({
            val effmaxgames = maxgames - 1
            if (gamelist.length > effmaxgames) {
              val sortedgames = gamelist.sortWith((a, b) => a.createdms > b.createdms)

              for (i <- 0 to gamelist.length - 1) {
                val g = sortedgames(i)
                if ((i >= effmaxgames) && (g.usergameid != "")) {
                  println("garbage collecting game " + i + " for " + handle + " created " + g.createdms + " id " + g.usergameid)
                  gameDAO.delete(g.usergameid)
                }
              }
            }
          })
      }
    }
    case _ => println("This was unexpected.")
  }
}

class MyScheduledActor @Inject() (
  val messagesApi: MessagesApi,
  viewDAO: models.daos.ViewDAO,
  tableDAO: models.daos.TableDAO,
  gameDAO: models.daos.GameDAO,
  userDAO: models.daos.UserDAO
) extends Actor with I18nSupport {
  tables.init(tableDAO, gameDAO, userDAO)

  val actorsystem = context.system

  val garbagecollector = actorsystem.actorOf(Props(GarbageCollector(gameDAO)), "garbagecollector")

  def broadcast_SendTableResultMessage(k: String, t: Table) {
    for (wri <- tables.getwrisfortable(k)) {
      SocketRequestMsg.sendMsg(wri.out, SendTableResultMessage(k, t, wri.webboardid))
    }
  }

  def broadcast_SitPlayerResultMessage(k: String, success: Boolean) {
    for (wri <- tables.getwrisfortable(k)) {
      SocketRequestMsg.sendMsg(wri.out, SitPlayerResultMessage(success, wri.webboardid))
    }
  }

  def receive = {
    case TickEvery1Second => {
      val timedoutlist = tables.gettimedoutlist
      tables.terminatetimedoutlist(timedoutlist)
      for (k <- timedoutlist) broadcast_SendTableResultMessage(k, tables.tables(k))
    }
    case QuartzTickEvery1Minute => {
      tables.handleidles
    }
    case srm: SocketRequestMsg => {
      srm.getMsg match {
        case HelloMessage(content) => srm.sendMsg(HelloMessage(
          s"Hi ${if (srm.user.isEmpty) "Anonymous" else srm.user.get.name.getOrElse("User")}, I received your message: " + content
        ))
        case CreateTableMessage(variant, timecontrol, webboardid) => {
          val result = tables.createandsave(variant, timecontrol)
          srm.sendMsg(TableCreationResultMessage(result._1))
          if (result._1) {
            val wriopt = tables.getwribyid(webboardid)
            if (!wriopt.isEmpty) {
              val wri = wriopt.get
              val newwri = WebBoardRegistryItem(webboardid, result._2, wri.out)
              tables.registerwebboard(newwri)
              SocketRequestMsg.sendMsg(wri.out, SendTableResultMessage(result._2, result._3, webboardid))
            }
          }
        }
        case StorePresentationMessage(presid: String, presg: Game) => {
          gameDAO.set(presid, presg).flatMap { setresult =>
            Future.successful({
              println("stored ok")
              srm.sendMsg(StorePresentationResultMessage(true))
            })
          }
        }
        case StoreViewMessage(id, content) => if (!srm.user.isEmpty) viewDAO.set(srm.user.get.userID.toString, id, content)
        case SendTablesMessage(which) => srm.sendMsg(SendTablesResultMessage(tables.tables))
        case DeleteTableMessage(k) => tables.delete(k)
        case SitPlayerMessage(k, i, player) => {
          if ((player.human) && (!srm.user.isEmpty)) {
            val user = srm.user.get
            player.uuid = user.userID
            player.rating = user.rating
            player.rd = user.rd
            player.lastrated = user.lastrated
          }
          val ok = tables.sitplayer(k, i, player)
          broadcast_SitPlayerResultMessage(k, ok)
          if (ok) {
            for (sk <- playermanagement_object.changedtables) {
              broadcast_SitPlayerResultMessage(sk, true)
            }
          }
        }
        case SendTableMessage(k) => {
          val (truek, table) = tables.gettable(k)
          if ((truek != null) && (table != null)) {
            broadcast_SendTableResultMessage(truek, table)
          }
        }
        case SendMoveMessage(k, algeb) => {
          if (!tables.tables(k).terminated) {
            val result = tables.makemove(k, algeb)
            if (result._1) {
              val t = tables.tables(k)
              broadcast_SendTableResultMessage(k, t)
              for (h <- t.humanhandles) {
                if (result._2) garbagecollector ! GarbageCollectGamesForUserMessage(h)
                val userwriopt = tables.getuserwribyid(h)
                if (!userwriopt.isEmpty) {
                  val userwri = userwriopt.get
                  val userwebboardid = userwri.webboardid
                  val wriopt = tables.getwribyid(userwebboardid)
                  if (!wriopt.isEmpty) {
                    val wri = wriopt.get
                    if (wri.tableid != k) {
                      println("loading ongoing game for " + h)
                      val newwri = WebBoardRegistryItem(userwebboardid, k, wri.out)
                      tables.registerwebboard(newwri)
                      SocketRequestMsg.sendMsg(wri.out, SendTableResultMessage(k, t, userwebboardid))
                    }
                  }
                }
              }
            }
          }
        }
        case RegisterWebBoardMessage(webboardid, tableid, handle) => {
          println("register " + webboardid + " tid " + tableid + " handle " + handle)
          val (truek, table) = tables.gettable(tableid, handle = handle)
          if ((truek != null) && (table != null)) {
            val wri = WebBoardRegistryItem(webboardid, truek, srm.out)
            tables.registerwebboard(wri)
            tables.registeruser(handle, wri)
            SocketRequestMsg.sendMsg(wri.out, SendTableResultMessage(truek, table, wri.webboardid))
          }
        }
        case MalformedMessage(content) => println("error: malformed socket request message")
        case _ => println("error: unexpected socket request message")
      }

    }
    case _ => println("This was unexpected.")
  }
}

case object TickEvery1Second
case object QuartzTickEvery1Minute

object SocketRequestMsg {
  def sendMsg(out: ActorRef, msg: Any) {
    val upicklestr = msg match {
      case x: HelloMessage => write(x)
      case x: TableCreationResultMessage => write(x)
      case x: SendTablesResultMessage => write(x)
      case x: SitPlayerResultMessage => write(x)
      case x: SendTableResultMessage => write(x)
      case x: RegisterWebBoardMessage => write(x)
      case x: StorePresentationResultMessage => write(x)
      case _ => ""
    }
    out ! (msg.getClass.getSimpleName + " " + upicklestr)
  }
}

case class SocketRequestMsg(msg: String, user: Option[models.User], out: ActorRef) {
  def getMsg: Any = {
    try {
      val firstspace = msg.indexOf(" ")
      val kind = msg.substring(0, firstspace)
      val content = msg.substring(firstspace + 1)
      kind match {
        case "HelloMessage" => return read[HelloMessage](content)
        case "CreateTableMessage" => return read[CreateTableMessage](content)
        case "StoreViewMessage" => return read[StoreViewMessage](content)
        case "SendTablesMessage" => return read[SendTablesMessage](content)
        case "DeleteTableMessage" => return read[DeleteTableMessage](content)
        case "SitPlayerMessage" => return read[SitPlayerMessage](content)
        case "SendTableMessage" => return read[SendTableMessage](content)
        case "SendMoveMessage" => return read[SendMoveMessage](content)
        case "RegisterWebBoardMessage" => return read[RegisterWebBoardMessage](content)
        case "StorePresentationMessage" => return read[StorePresentationMessage](content)
      }
    } catch { case e: Throwable => MalformedMessage(msg) }
  }
  def sendMsg(msg: Any) {
    SocketRequestMsg.sendMsg(out, msg)
  }

  def handle = if (!user.isEmpty) user.get.handle.getOrElse("") else ""
}