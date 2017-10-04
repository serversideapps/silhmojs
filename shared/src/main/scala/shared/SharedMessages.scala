package shared

//////////////////////////////////////////////////////////////

// general

case class MalformedMessage(content: String)

case class HelloMessage(content: String)

//////////////////////////////////////////////////////////////

// requests

case class CreateTableMessage(variant: String, timecontrol: String, webboardid: String)

case class StoreViewMessage(id: String, content: String)

case class SendTablesMessage(which: String)

case class DeleteTableMessage(k: String)

case class SitPlayerMessage(k: String, i: Int, player: Player)

case class SendTableMessage(k: String)

case class SendMoveMessage(k: String, algeb: String)

case class RegisterWebBoardMessage(webboardid: String, tableid: String, handle: String)

case class StorePresentationMessage(presid: String, presg: Game)

//////////////////////////////////////////////////////////////

// responses

case class TableCreationResultMessage(result: Boolean)

case class SendTablesResultMessage(tables: Map[String, Table])

case class SitPlayerResultMessage(success: Boolean, webboardid: String)

case class SendTableResultMessage(k: String, table: Table, webboardid: String)

case class StorePresentationResultMessage(success: Boolean)