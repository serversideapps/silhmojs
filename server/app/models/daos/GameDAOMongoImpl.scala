package models.daos

import javax.inject.Inject

import scala.concurrent.Future

import play.modules.reactivemongo.ReactiveMongoApi

import reactivemongo.api._
import reactivemongo.api.commands._
import reactivemongo.api.collections.bson._

import scala.concurrent.ExecutionContext.Implicits.global

import shared._

class GameDAOMongoImpl @Inject() (reactiveMongoApi: ReactiveMongoApi) extends GameDAO {

  implicit val reader = new reactivemongo.bson.BSONDocumentReader[Game] {
    def read(bson: reactivemongo.bson.BSONDocument): Game = {
      val presstr = bson.getAs[String]("presentation").getOrElse("")
      val result = new Game(
        usergameid = bson.getAs[String]("usergameid").getOrElse(""),
        createdms = (bson.getAs[String]("createdms").getOrElse("0.0")).toDouble,
        White = bson.getAs[String]("White").getOrElse(""),
        Black = bson.getAs[String]("Black").getOrElse(""),
        Yellow = bson.getAs[String]("Yellow").getOrElse(""),
        Red = bson.getAs[String]("Red").getOrElse(""),
        Result = bson.getAs[String]("Result").getOrElse(""),
        Date = bson.getAs[String]("Date").getOrElse(""),
        Time = bson.getAs[String]("Time").getOrElse(""),
        player0 = bson.getAs[String]("player0").getOrElse(""),
        player1 = bson.getAs[String]("player1").getOrElse(""),
        player2 = bson.getAs[String]("player2").getOrElse(""),
        player3 = bson.getAs[String]("player3").getOrElse(""),
        pgn = bson.getAs[String]("pgn").getOrElse(""),
        variant = bson.getAs[String]("variant").getOrElse(""),
        presentationid = bson.getAs[String]("presentationid").getOrElse(""),
        presentationowner = bson.getAs[String]("presentationowner").getOrElse(""),
        presentationtitle = bson.getAs[String]("presentationtitle").getOrElse(""),
        presentationcandelete = bson.getAs[String]("presentationcandelete").getOrElse(""),
        presentationcanedit = bson.getAs[String]("presentationcanedit").getOrElse(""),
        presentation = if (presstr != "") upickle.default.read[Presentation](presstr) else Presentation()
      )
      result
    }
  }

  val hash = new utils.misc.MongoSimpleHashMap[String, Game](
    "games",
    (id: String) => id,
    new reactivemongo.bson.BSONDocumentWriter[Game] {
      def write(game: Game): reactivemongo.bson.BSONDocument =
        reactivemongo.bson.BSONDocument(
          "_id" -> game.effectiveid,
          "usergameid" -> game.usergameid,
          "createdms" -> ("" + game.createdms),
          "White" -> game.White,
          "Black" -> game.Black,
          "Yellow" -> game.Yellow,
          "Red" -> game.Red,
          "Result" -> game.Result,
          "Date" -> game.Date,
          "Time" -> game.Time,
          "player0" -> game.player0,
          "player1" -> game.player1,
          "player2" -> game.player2,
          "player3" -> game.player3,
          "pgn" -> game.pgn,
          "variant" -> game.variant,
          "presentationid" -> game.presentation.id,
          "presentationowner" -> game.presentation.owner,
          "presentationtitle" -> game.presentation.title,
          "presentationcandelete" -> game.presentation.candelete,
          "presentationcanedit" -> game.presentation.canedit,
          "presentation" -> upickle.default.write[Presentation](game.presentation)
        )
    },
    reader,
    reactiveMongoApi
  )

  def get(id: String): Future[Option[Game]] = hash.find(id)

  def set(id: String, game: Game): Future[Any] = hash.update(id, game).flatMap { _ => Future.successful(game) }

  def delete(id: String): Future[Any] = hash.delete(id)

  def find(handle: String, max: Int = settings.GlobalSettings.MONGO_COLLECT_MAX): Future[List[Game]] = hash.mongoCollection.flatMap(
    collection => collection.find(
      {
        val BD = reactivemongo.bson.BSONDocument
        val BA = reactivemongo.bson.BSONArray
        BD("$or" -> BA(
          BD("player0" -> handle),
          BD("player1" -> handle),
          BD("player2" -> handle),
          BD("player3" -> handle)
        ))
      }
    ).cursor[Game]().collect[List](max, Cursor.FailOnError[List[Game]]())
  )

  def findpres(handle: String, max: Int = settings.GlobalSettings.MONGO_COLLECT_MAX): Future[List[Game]] = hash.mongoCollection.flatMap(
    collection => collection.find(
      reactivemongo.bson.BSONDocument("presentationowner" -> handle)
    ).cursor[Game]().collect[List](max, Cursor.FailOnError[List[Game]]())
  )

}
