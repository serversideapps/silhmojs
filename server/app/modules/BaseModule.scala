package modules

import com.google.inject.AbstractModule
import models.daos._
import models.services._
import net.codingwell.scalaguice.ScalaModule

/**
 * The base Guice module.
 */
class BaseModule extends AbstractModule with ScalaModule {

  /**
   * Configures the module.
   */
  def configure(): Unit = {
    bind[AuthTokenDAO].to[AuthTokenDAOMongoImpl]
    bind[AuthTokenService].to[AuthTokenServiceImpl]
    bind[EnvDAO].to[EnvDAOImpl]
    bind[SettingDAO].to[SettingDAOMongoImpl]
    bind[ViewDAO].to[ViewDAOMongoImpl]
    bind[TableDAO].to[TableDAOMongoImpl]
    bind[GameDAO].to[GameDAOMongoImpl]
    bind[myform.MyFormConfig].asEagerSingleton()
  }
}
