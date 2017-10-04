package models.daos

import java.util.UUID

import scala.concurrent.Future

import javax.inject.Inject

import play.modules.reactivemongo.ReactiveMongoApi

import scala.concurrent.ExecutionContext.Implicits.global

import org.joda.time.DateTime
import models.AuthToken

class AuthTokenDAOMongoImpl @Inject() (reactiveMongoApi: ReactiveMongoApi) extends AuthTokenDAO {

  implicit val reader = new reactivemongo.bson.BSONDocumentReader[AuthToken] {
    def read(bson: reactivemongo.bson.BSONDocument): AuthToken = {
      val result = new AuthToken(
        id = java.util.UUID.fromString(bson.getAs[String]("id").getOrElse("")),
        userID = java.util.UUID.fromString(bson.getAs[String]("uid").getOrElse("")),
        expiry = DateTime.parse(bson.getAs[String]("expiry").getOrElse(""))
      )
      result
    }
  }

  val hash = new utils.misc.MongoSimpleHashMap[java.util.UUID, AuthToken](
    "tokens",
    (uuid: java.util.UUID) => uuid.toString,
    new reactivemongo.bson.BSONDocumentWriter[AuthToken] {
      def write(token: AuthToken): reactivemongo.bson.BSONDocument =
        reactivemongo.bson.BSONDocument(
          "id" -> token.id.toString(),
          "uid" -> token.userID.toString(),
          "expiry" -> token.expiry.toString()
        )
    },
    reader,
    reactiveMongoApi
  )

  def findall() = hash.findall()

  def find(id: UUID) = hash.find(id)

  def findExpired(dateTime: DateTime) = Future.successful { Seq[models.AuthToken]() }

  def save(token: AuthToken) = hash.update(token.id, token).map(opttoken => token)

  def remove(id: UUID) = hash.delete(id).map(_ => Unit)
}
