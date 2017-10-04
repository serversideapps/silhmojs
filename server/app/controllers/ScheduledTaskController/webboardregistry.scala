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

case class WebBoardRegistryItem(
  webboardid: String = "",
  tableid: String = "",
  out: ActorRef = null
) {

}

object webboardregistry_object {
  import tables._

  var items = Map[String, WebBoardRegistryItem]()

  var useritems = Map[String, WebBoardRegistryItem]()

  def getwribyid(webboardid: String): Option[WebBoardRegistryItem] = {
    if (!items.contains(webboardid)) return None
    Some(items(webboardid))
  }

  def getuserwribyid(handle: String): Option[WebBoardRegistryItem] = {
    if (!useritems.contains(handle)) return None
    Some(useritems(handle))
  }

  def registerwebboard(wri: WebBoardRegistryItem) {
    items += (wri.webboardid -> wri)
  }

  def registeruser(handle: String, wri: WebBoardRegistryItem) {
    if (handle == "") return

    useritems += (handle -> wri)
  }

  def getwrisfortable(k: String): List[WebBoardRegistryItem] = (for ((id, item) <- items if (item.tableid == k)) yield item).toList

}