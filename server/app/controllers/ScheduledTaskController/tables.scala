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

object tables {

  ///////////////////////////////////////////////
  // variables

  var tableDAO: models.daos.TableDAO = null
  var gameDAO: models.daos.GameDAO = null
  var userDAO: models.daos.UserDAO = null
  var tables = Map[String, Table]()
  var games = Map[String, game]()
  var initialized = false

  ///////////////////////////////////////////////

  ////////////////////////////////////////////////
  // initialization

  def readtables {
    tableDAO.getall().onComplete {
      case Success(jsons) =>
        tables = jsons.map(json => read[Table](json)).
          map(table => Tuple2[String, Table](table.id, table)).toMap
        initialized = true
        println("[ tables init ] success [ " + tables.keys.mkString(",") + " ]")
      case _ => println("[ tables init ] failed")
    }
  }

  def init(
    setTableDAO: models.daos.TableDAO,
    setGameDAO: models.daos.GameDAO,
    setUserDAO: models.daos.UserDAO
  ) {
    if (initialized) return
    movetable.init("Standard")
    tableDAO = setTableDAO
    gameDAO = setGameDAO
    userDAO = setUserDAO
    readtables

    board4.init
  }

  ////////////////////////////////////////////////

  ////////////////////////////////////////////////
  // exported

  // make move
  def makemove(k: String, algeb: String): Tuple2[Boolean, Boolean] = makemove_object.makemove(k, algeb)
  // terminate table
  def terminate(k: String, action: String, setresultreason: String = "") = terminatetable_object.terminate(k, action, setresultreason)
  // utils
  def gettable(k: String, updatetime: Boolean = true, handle: String = ""): Tuple2[String, Table] = tableutils_object.gettable(k, updatetime, handle)
  def getb(t: Table): board = tableutils_object.getb(t)
  def getturni(t: Table): Int = tableutils_object.getturni(t)
  def getnumplayers(t: Table): Int = tableutils_object.getnumplayers(t)
  def resultvalue(pgnresult: String): Double = tableutils_object.resultvalue(pgnresult)
  def sorttablekeysfunc(keya: String, keyb: String): Boolean = tableutils_object.sorttablekeysfunc(keya, keyb)
  def tablekeyssorted = tables.keys.toList.sortWith(sorttablekeysfunc)
  val r = new scala.util.Random
  // time management
  def updateplayertime(t: Table, i: Int) = timemanagement_object.updateplayertime(t, i)
  def istimedout(t: Table): Boolean = timemanagement_object.istimedout(t)
  def gettimedoutlist: List[String] = timemanagement_object.gettimedoutlist
  def terminatetimedoutlist(ks: List[String]) = timemanagement_object.terminatetimedoutlist(ks)
  // create table
  def createId: String = createtable_object.createId
  def create(variant: String = smartchess.board.DEFAULT_VARIANT, timecontrol: String = DEFAULT_TIME_CONTROL): Tuple3[Boolean, String, Table] = createtable_object.create(variant, timecontrol)
  def createandsave(variant: String = smartchess.board.DEFAULT_VARIANT, timecontrol: String = DEFAULT_TIME_CONTROL): Tuple3[Boolean, String, Table] = createtable_object.createandsave(variant, timecontrol)
  def delete(k: String) = createtable_object.delete(k)
  def handleidles = createtable_object.handleidles
  def isDuplicate(t: Table): Boolean = createtable_object.isDuplicate(t)
  // player management
  def sitplayer(k: String, i: Int, player: Player): Boolean = playermanagement_object.sitplayer(k, i, player)
  def updaterating(uuid: java.util.UUID, g: GlickoData) = playermanagement_object.updaterating(uuid, g)
  // webboard registry
  def registerwebboard(wri: WebBoardRegistryItem) = webboardregistry_object.registerwebboard(wri)
  def registeruser(handle: String, wri: WebBoardRegistryItem) = webboardregistry_object.registeruser(handle, wri)
  def getwrisfortable(k: String): List[WebBoardRegistryItem] = webboardregistry_object.getwrisfortable(k)
  def getwribyid(webboardid: String): Option[WebBoardRegistryItem] = webboardregistry_object.getwribyid(webboardid)
  def getuserwribyid(handle: String): Option[WebBoardRegistryItem] = webboardregistry_object.getuserwribyid(handle)

  ////////////////////////////////////////////////

}