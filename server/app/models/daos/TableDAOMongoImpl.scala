package models.daos

import javax.inject.Inject

import java.util.UUID

import scala.concurrent.Future

import play.modules.reactivemongo.ReactiveMongoApi

import reactivemongo.api._
import reactivemongo.api.collections.bson._

import scala.concurrent.ExecutionContext.Implicits.global

import utils.misc._

class TableDAOMongoImpl @Inject() (reactiveMongoApi: ReactiveMongoApi) extends TableDAO {

  val hash = new utils.misc.MongoStringHashMap(
    "tables",
    reactiveMongoApi
  )

  def getall(): Future[List[String]] = hash.getall().map(l => for (s <- l) yield s.content)

  def set(id: String, content: String) = hash.set(id, content)

  def delete(id: String): Future[Any] = hash.delete(id)

}