package models.daos

import javax.inject.Inject

import java.util.UUID

import scala.concurrent.Future

import play.modules.reactivemongo.ReactiveMongoApi

import reactivemongo.api._
import reactivemongo.api.collections.bson._

import scala.concurrent.ExecutionContext.Implicits.global

import utils.misc._

class ViewDAOMongoImpl @Inject() (reactiveMongoApi: ReactiveMongoApi) extends ViewDAO {

  val hash = new utils.misc.MongoDoubleKeyedStringHashMap(
    "views",
    reactiveMongoApi
  )

  def get(id1: String, id2: String): Future[Option[StringWithIds]] = hash.get(id1, id2)

  def set(id1: String, id2: String, content: String) = hash.set(id1, id2, content)

}