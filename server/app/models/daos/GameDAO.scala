package models.daos

import scala.concurrent.Future

import shared._

trait GameDAO {

  def get(id: String): Future[Option[Game]]

  def set(id: String, g: Game): Future[Any]

  def delete(id: String): Future[Any]

  def find(handle: String, max: Int = settings.GlobalSettings.MONGO_COLLECT_MAX): Future[List[Game]]

  def findpres(handle: String, max: Int = settings.GlobalSettings.MONGO_COLLECT_MAX): Future[List[Game]]

}
