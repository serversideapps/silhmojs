package models.daos

import javax.inject.Inject

import java.util.UUID

import scala.concurrent.Future

import play.modules.reactivemongo.ReactiveMongoApi

import scala.concurrent.ExecutionContext.Implicits.global

import com.mohiva.play.silhouette.api.LoginInfo
import models.User

import utils.misc.Glicko

class UserDAOMongoImpl @Inject() (reactiveMongoApi: ReactiveMongoApi) extends UserDAO {

  implicit val reader = new reactivemongo.bson.BSONDocumentReader[User] {
    def read(bson: reactivemongo.bson.BSONDocument): User = {
      val result = new User(
        userID = java.util.UUID.fromString(bson.getAs[String]("id").getOrElse("")),
        loginInfo = LoginInfo(
          bson.getAs[String]("liprid").getOrElse("credentials"),
          bson.getAs[String]("liprk").getOrElse("")
        ),
        handle = bson.getAs[String]("handle"),
        regpass = bson.getAs[String]("regpass"),
        firstName = bson.getAs[String]("firstname"),
        lastName = bson.getAs[String]("lastname"),
        fullName = bson.getAs[String]("fullname"),
        email = bson.getAs[String]("email"),
        avatarURL = bson.getAs[String]("avatarurl"),
        activated = bson.getAs[Boolean]("activated").getOrElse(false),
        admin = bson.getAs[Boolean]("admin").getOrElse(false),
        rating = bson.getAs[Double]("rating").getOrElse(Glicko.rating0),
        rd = bson.getAs[Double]("rd").getOrElse(Glicko.rd0),
        lastrated = bson.getAs[Double]("lastrated").getOrElse(0.0)
      )
      result
    }
  }

  val hash = new utils.misc.MongoSimpleHashMap[java.util.UUID, User](
    "users",
    (uuid: java.util.UUID) => uuid.toString,
    new reactivemongo.bson.BSONDocumentWriter[User] {
      def write(user: User): reactivemongo.bson.BSONDocument =
        reactivemongo.bson.BSONDocument(
          "id" -> user.userID.toString(),
          "liprid" -> user.loginInfo.providerID,
          "liprk" -> user.loginInfo.providerKey,
          "handle" -> user.handle,
          "regpass" -> user.regpass,
          "firstname" -> user.firstName,
          "lastname" -> user.lastName,
          "fullname" -> user.fullName,
          "email" -> user.email,
          "avatarurl" -> user.avatarURL,
          "activated" -> user.activated,
          "admin" -> user.admin,
          "rating" -> user.rating,
          "rd" -> user.rd,
          "lastrated" -> user.lastrated
        )
    },
    reader,
    reactiveMongoApi
  )

  def findall(): Future[List[User]] = hash.findall()

  def find(loginInfo: LoginInfo): Future[Option[User]] = for {
    collection <- hash.mongoCollection
    doc <- {
      println(s"[ users ] find " + loginInfo)
      collection.find(reactivemongo.bson.BSONDocument(
        "liprid" -> loginInfo.providerID,
        "liprk" -> loginInfo.providerKey
      )).one[User]
    }
  } yield doc

  def find(handle: String): Future[Option[User]] = for {
    collection <- hash.mongoCollection
    doc <- {
      println(s"[ users ] find " + handle)
      collection.find(reactivemongo.bson.BSONDocument(
        "handle" -> handle
      )).one[User]
    }
  } yield doc

  def find(userID: UUID) = hash.find(userID)

  def save(user: User) = hash.update(user.userID, user).flatMap { _ => Future.successful(user) }
}
