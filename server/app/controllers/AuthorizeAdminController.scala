package controllers

import java.util.UUID
import javax.inject.Inject

import com.mohiva.play.silhouette.api._
import com.mohiva.play.silhouette.api.repositories.AuthInfoRepository
import com.mohiva.play.silhouette.api.util.{ PasswordHasherRegistry, PasswordInfo }
import com.mohiva.play.silhouette.impl.providers.CredentialsProvider
import models.services.{ AuthTokenService, UserService }
import models.daos._
import play.api.i18n.{ I18nSupport, Messages, MessagesApi }
import play.api.libs.concurrent.Execution.Implicits._
import play.api.mvc.Controller
import utils.auth.DefaultEnv

import scala.concurrent.Future

class AuthorizeAdminController @Inject() (
  val messagesApi: MessagesApi,
  silhouette: Silhouette[DefaultEnv],
  userService: UserService,
  authInfoRepository: AuthInfoRepository,
  passwordHasherRegistry: PasswordHasherRegistry,
  authTokenService: AuthTokenService,
  envDAO: EnvDAO,
  userDAO: models.daos.UserDAO
)
  extends Controller with I18nSupport {

  def view = silhouette.SecuredAction.async { implicit request =>
    Future.successful(Ok(views.html.authorizeAdmin(new myform.MyAuthorizeAdminForm())))
  }

  def submit = silhouette.SecuredAction.async { implicit request =>
    new myform.MyAuthorizeAdminForm().bindFromRequest match {
      case form: myform.MyAuthorizeAdminForm => Future.successful(BadRequest(views.html.authorizeAdmin(form)))
      case data: myform.MyAuthorizeAdminFormData => {
        val adminpass = envDAO.getAdminPass
        if (adminpass == data.password) {
          val user = request.identity.copy(admin = true)
          userDAO.save(user)
          Future.successful(Ok(views.html.home(user)))
        } else Future.successful(Redirect(routes.AuthorizeAdminController.view()))
      }
    }
  }

}
