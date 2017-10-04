package controllers

import javax.inject.Inject

import com.mohiva.play.silhouette.api._
import com.mohiva.play.silhouette.api.exceptions.ProviderException
import com.mohiva.play.silhouette.api.repositories.AuthInfoRepository
import com.mohiva.play.silhouette.api.util.{ Credentials, PasswordHasherRegistry, PasswordInfo }
import com.mohiva.play.silhouette.impl.providers.CredentialsProvider
import models.services.UserService
import play.api.i18n.{ I18nSupport, Messages, MessagesApi }
import play.api.libs.concurrent.Execution.Implicits._
import play.api.mvc.Controller
import utils.auth.{ DefaultEnv, WithProvider }

import scala.concurrent.Future

class ChangePasswordController @Inject() (
  val messagesApi: MessagesApi,
  silhouette: Silhouette[DefaultEnv],
  userService: UserService,
  credentialsProvider: CredentialsProvider,
  authInfoRepository: AuthInfoRepository,
  passwordHasherRegistry: PasswordHasherRegistry
)
  extends Controller with I18nSupport {

  def view = silhouette.SecuredAction(WithProvider[DefaultEnv#A](CredentialsProvider.ID)) { implicit request =>
    Ok(views.html.changePassword(new myform.MyChangePasswordForm(), request.identity))
  }

  def submit = silhouette.SecuredAction(WithProvider[DefaultEnv#A](CredentialsProvider.ID)).async { implicit request =>
    new myform.MyChangePasswordForm().bindFromRequest match {
      case form: myform.MyChangePasswordForm => Future.successful(BadRequest(views.html.changePassword(form, request.identity)))
      case data: myform.MyChangePasswordFormData => {
        val (currentPassword, newPassword) = (data.currentPassword, data.newPassword)
        val credentials = Credentials(request.identity.email.getOrElse(""), currentPassword)
        credentialsProvider.authenticate(credentials).flatMap { loginInfo =>
          val passwordInfo = passwordHasherRegistry.current.hash(newPassword)
          authInfoRepository.update[PasswordInfo](loginInfo, passwordInfo).map { _ =>
            Redirect(routes.ChangePasswordController.view()).flashing("success" -> Messages("password.changed"))
          }
        }.recover {
          case e: ProviderException =>
            Redirect(routes.ChangePasswordController.view()).flashing("error" -> Messages("current.password.invalid"))
        }
      }
    }
  }
}
