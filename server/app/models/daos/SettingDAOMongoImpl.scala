package models.daos

import javax.inject.Inject

import java.util.UUID

import scala.concurrent.Future

import play.modules.reactivemongo.ReactiveMongoApi

import reactivemongo.api._
import reactivemongo.api.collections.bson._

import scala.concurrent.ExecutionContext.Implicits.global

import shared.Setting

class SettingDAOMongoImpl @Inject() (reactiveMongoApi: ReactiveMongoApi) extends SettingDAO {

  val hash = new utils.misc.MongoSimpleHashMap[java.util.UUID, Setting](
    "settings",
    (uuid: java.util.UUID) => uuid.toString,
    new reactivemongo.bson.BSONDocumentWriter[Setting] {
      def write(setting: Setting): reactivemongo.bson.BSONDocument =
        reactivemongo.bson.BSONDocument(
          "piecetype" -> setting.piecetype,
          "note" -> setting.note
        )
    },
    new reactivemongo.bson.BSONDocumentReader[Setting] {
      def read(bson: reactivemongo.bson.BSONDocument): Setting = {
        val result = Setting(
          piecetype = bson.getAs[String]("piecetype"),
          note = bson.getAs[String]("note")
        )
        result
      }
    },
    reactiveMongoApi
  )

  def get(settingID: java.util.UUID): Future[Option[Setting]] = hash.find(settingID)

  def set(settingID: java.util.UUID, setting: Setting) = hash.update(settingID, setting)

  def delete(settingID: java.util.UUID) = hash.delete(settingID)

}