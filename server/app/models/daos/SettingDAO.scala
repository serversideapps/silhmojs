package models.daos

import scala.concurrent.Future

import shared.Setting

trait SettingDAO {

  def get(settingID: java.util.UUID): Future[Option[Setting]]

  def set(settingID: java.util.UUID, setting: Setting): Future[Any]

  def delete(settingID: java.util.UUID): Future[Any]

}
