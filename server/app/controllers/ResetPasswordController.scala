package controllers

import java.util.UUID
import javax.inject.Inject

import com.mohiva.play.silhouette.api._
import com.mohiva.play.silhouette.api.repositories.AuthInfoRepository
import com.mohiva.play.silhouette.api.util.{ PasswordHasherRegistry, PasswordInfo }
import com.mohiva.play.silhouette.impl.providers.CredentialsProvider
import models.services.{ AuthTokenService, UserService }
import play.api.i18n.{ I18nSupport, Messages, MessagesApi }
import play.api.libs.concurrent.Execution.Implicits._
import play.api.mvc.Controller
import utils.auth.DefaultEnv

import scala.concurrent.Future

class ResetPasswordController @Inject() (
  val messagesApi: MessagesApi,
  silhouette: Silhouette[DefaultEnv],
  userService: UserService,
  authInfoRepository: AuthInfoRepository,
  passwordHasherRegistry: PasswordHasherRegistry,
  authTokenService: AuthTokenService
)
  extends Controller with I18nSupport {

  def view(token: UUID) = silhouette.UnsecuredAction.async { implicit request =>
    authTokenService.validate(token).map {
      case Some(authToken) => Ok(views.html.resetPassword(new myform.MyResetPasswordForm(), token))
      case None => Redirect(routes.SignInController.view()).flashing("error" -> Messages("invalid.reset.link"))
    }
  }

  def submit(token: UUID) = silhouette.UnsecuredAction.async { implicit request =>
    authTokenService.validate(token).flatMap {
      case Some(authToken) =>
        new myform.MyResetPasswordForm().bindFromRequest match {
          case form: myform.MyResetPasswordForm => Future.successful(BadRequest(views.html.resetPassword(form, token)))
          case data: myform.MyResetPasswordFormData => userService.retrieve(authToken.userID).flatMap {
            case Some(user) if user.loginInfo.providerID == CredentialsProvider.ID =>
              val passwordInfo = passwordHasherRegistry.current.hash(data.password)
              authInfoRepository.update[PasswordInfo](user.loginInfo, passwordInfo).map { _ =>
                Redirect(routes.SignInController.view()).flashing("success" -> Messages("password.reset"))
              }
            case _ => Future.successful(Redirect(routes.SignInController.view()).flashing("error" -> Messages("invalid.reset.link")))
          }
        }
      case None => Future.successful(Redirect(routes.SignInController.view()).flashing("error" -> Messages("invalid.reset.link")))
    }
  }
}
