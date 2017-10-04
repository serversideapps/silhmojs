package models

import java.util.UUID

import com.mohiva.play.silhouette.api.{ Identity, LoginInfo }

import utils.misc.Glicko

case class User(
  userID: UUID,
  loginInfo: LoginInfo,
  handle: Option[String],
  regpass: Option[String],
  firstName: Option[String],
  lastName: Option[String],
  fullName: Option[String],
  email: Option[String],
  avatarURL: Option[String],
  activated: Boolean,
  admin: Boolean = false,
  rating: Double = Glicko.rating0,
  rd: Double = Glicko.rd0,
  lastrated: Double = 0.0
) extends Identity {

  def name = fullName.orElse {
    firstName -> lastName match {
      case (Some(f), Some(l)) => Some(f + " " + l)
      case (Some(f), None) => Some(f)
      case (None, Some(l)) => Some(l)
      case _ => None
    }
  }

  def setActivated(a: Boolean) = this.copy(activated = a)
}
