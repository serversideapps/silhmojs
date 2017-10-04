package models.daos

import play.api._

import javax.inject.Inject

class EnvDAOImpl @Inject() (c: Configuration) extends EnvDAO {

  def getDosendmail: Boolean = c.getBoolean("env_DO_SEND_MAIL").getOrElse(false)

  def getDocaptcha: Boolean = c.getBoolean("env_DO_CAPTCHA").getOrElse(true)

  def getAdminPass: String = c.getString("env_ADMIN_PASS").getOrElse("x")

}
