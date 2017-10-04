package models.daos

import scala.concurrent.Future

import utils.misc._

trait ViewDAO {

  def get(id1: String, id2: String): Future[Option[StringWithIds]]

  def set(id1: String, id2: String, content: String): Future[Any]

}
