package utils.misc

import javax.inject.Inject

import scala.concurrent.Future

import scala.concurrent.ExecutionContext.Implicits.global

import play.modules.reactivemongo.ReactiveMongoApi

import reactivemongo.api._
import reactivemongo.api.commands._
import reactivemongo.api.collections.bson._
import reactivemongo.bson._

class MongoSimpleHashMap[K, T](
  name: String,
  keytranslator: (K) => String,
  implicit val writer: BSONDocumentWriter[T],
  implicit val reader: BSONDocumentReader[T],
  reactiveMongoApi: ReactiveMongoApi
) {
  def mongoCollection = reactiveMongoApi.database.map(_.collection[BSONCollection](name))

  println(s"[ $name ] constructed")

  def insert(doc: T): Future[WriteResult] = for {
    collection <- mongoCollection
    wres <- {
      println(s"[ $name ] insert " + doc)
      collection.insert(doc)
    }
  } yield wres

  def find(id: K): Future[Option[T]] = for {
    collection <- mongoCollection
    doc <- {
      println(s"[ $name ] find " + id)
      collection.find(BSONDocument("_id" -> keytranslator(id))).one[T]
    }
  } yield doc

  def update(id: K, doc: T, upsert: Boolean = true): Future[WriteResult] = for {
    collection <- mongoCollection
    wres <- {
      println(s"[ $name ] update ( upsert = $upsert ) " + id + " : " + doc)
      collection.update(BSONDocument("_id" -> keytranslator(id)), doc, upsert = upsert)
    }
  } yield wres

  def delete(id: K): Future[WriteResult] = for {
    collection <- mongoCollection
    wres <- {
      println(s"[ $name ] delete " + id)
      collection.remove(BSONDocument("_id" -> keytranslator(id)))
    }
  } yield wres

  def findall(max: Int = settings.GlobalSettings.MONGO_COLLECT_MAX): Future[List[T]] = mongoCollection.flatMap(
    collection => collection.find(BSONDocument()).cursor[T]().collect[List](max, Cursor.FailOnError[List[T]]())
  )

}

case class StringWithIds(
  id1: String,
  id2: String,
  content: String
)

class MongoDoubleKeyedStringHashMap(
  name: String,
  reactiveMongoApi: ReactiveMongoApi
) {
  def mongoCollection = reactiveMongoApi.database.map(_.collection[BSONCollection](name))

  implicit object writer extends reactivemongo.bson.BSONDocumentWriter[StringWithIds] {
    def write(swid: StringWithIds): reactivemongo.bson.BSONDocument =
      reactivemongo.bson.BSONDocument(
        "id1" -> swid.id1,
        "id2" -> swid.id2,
        "content" -> swid.content
      )
  }
  implicit object reader extends reactivemongo.bson.BSONDocumentReader[StringWithIds] {
    def read(bson: reactivemongo.bson.BSONDocument): StringWithIds = {
      val result = StringWithIds(
        id1 = bson.getAs[String]("id1").getOrElse("id1"),
        id2 = bson.getAs[String]("id2").getOrElse("id2"),
        content = bson.getAs[String]("content").getOrElse("content")
      )
      result
    }
  }

  println(s"[ $name ] constructed")

  def get(id1: String, id2: String): Future[Option[StringWithIds]] = for {
    collection <- mongoCollection
    doc <- {
      println(s"[ $name ] get $id1 , $id2")
      collection.find(BSONDocument("id1" -> id1, "id2" -> id2)).one[StringWithIds]
    }
  } yield doc

  def set(id1: String, id2: String, content: String): Future[WriteResult] = for {
    collection <- mongoCollection
    wres <- {
      println(s"[ $name ] set $id1 , $id2 : " + content)
      collection.update(BSONDocument("id1" -> id1, "id2" -> id2), StringWithIds(id1, id2, content), upsert = true)
    }
  } yield wres

}

case class JSONString(content: String)

class MongoStringHashMap(
  name: String,
  reactiveMongoApi: ReactiveMongoApi
) {
  def mongoCollection = reactiveMongoApi.database.map(_.collection[BSONCollection](name))

  implicit object writer extends reactivemongo.bson.BSONDocumentWriter[JSONString] {
    def write(content: JSONString): reactivemongo.bson.BSONDocument =
      reactivemongo.bson.BSONDocument(
        "content" -> content.content
      )
  }
  implicit object reader extends reactivemongo.bson.BSONDocumentReader[JSONString] {
    def read(bson: reactivemongo.bson.BSONDocument): JSONString = {
      val result = JSONString(bson.getAs[String]("content").getOrElse("content"))
      result
    }
  }

  println(s"[ $name ] constructed")

  def getall(max: Int = settings.GlobalSettings.MONGO_COLLECT_MAX): Future[List[JSONString]] = mongoCollection.flatMap(
    collection => collection.find(BSONDocument()).cursor[JSONString]().collect[List](max, Cursor.FailOnError[List[JSONString]]())
  )

  def set(id: String, content: String): Future[WriteResult] = for {
    collection <- mongoCollection
    wres <- {
      println(s"[ $name ] set $id : " + content)
      collection.update(BSONDocument("_id" -> id), JSONString(content), upsert = true)
    }
  } yield wres

  def delete(id: String): Future[WriteResult] = for {
    collection <- mongoCollection
    wres <- {
      println(s"[ $name ] delete " + id)
      collection.remove(BSONDocument("_id" -> id))
    }
  } yield wres

}
