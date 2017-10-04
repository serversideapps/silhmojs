package models.daos

import scala.concurrent.Future

trait TableDAO {

  def getall(): Future[List[String]]

  def set(id: String, content: String): Future[Any]

  def delete(id: String): Future[Any]

}
