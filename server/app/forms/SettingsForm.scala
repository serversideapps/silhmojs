package forms

import play.api.data.Form
import play.api.data.Forms._

object SettingsForm {

  val form = Form(
    mapping(
      "piecetype" -> nonEmptyText,
      "note" -> text
    )(Data.apply)(Data.unapply)
  )

  case class Data(
    piecetype: String,
    note: String
  )
}
