package models.daos

trait EnvDAO {

  def getDosendmail: Boolean

  def getDocaptcha: Boolean

  def getAdminPass: String

}
