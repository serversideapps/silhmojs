package controllers

import javax.inject.Inject

import com.mohiva.play.silhouette.api._
import com.mohiva.play.silhouette.impl.providers.CredentialsProvider
import models.services.{ AuthTokenService, UserService }
import models.daos._
import play.api.i18n.{ I18nSupport, Messages, MessagesApi }
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.mailer.{ Email, MailerClient }
import play.api.mvc.Controller
import utils.auth.DefaultEnv

import scala.concurrent.Future

class ForgotPasswordController @Inject() (
  val messagesApi: MessagesApi,
  silhouette: Silhouette[DefaultEnv],
  userService: UserService,
  authTokenService: AuthTokenService,
  mailerClient: MailerClient,
  envDAO: EnvDAO
)
  extends Controller with I18nSupport {

  def view = silhouette.UnsecuredAction.async { implicit request =>
    Future.successful(Ok(views.html.forgotPassword(new myform.MyForgotPasswordForm())))
  }

  def submit = silhouette.UnsecuredAction.async { implicit request =>
    new myform.MyForgotPasswordForm().bindFromRequest match {
      case form: myform.MyForgotPasswordForm => Future.successful(BadRequest(views.html.forgotPassword(form)))
      case data: myform.MyForgotPasswordFormData => {
        val email = data.email
        val loginInfo = LoginInfo(CredentialsProvider.ID, email)
        val result = Redirect(routes.SignInController.view()).flashing("info" -> Messages("reset.email.sent"))
        userService.retrieve(loginInfo).flatMap {
          case Some(user) if user.email.isDefined =>
            authTokenService.create(user.userID).map { authToken =>
              val url = routes.ResetPasswordController.view(authToken.id).absoluteURL()
              println("url " + url)
              if (envDAO.getDosendmail) mailerClient.send(Email(
                subject = Messages("email.reset.password.subject"),
                from = Messages("email.from"),
                to = Seq(email),
                bodyText = Some(views.txt.emails.resetPassword(user, url).body),
                bodyHtml = Some(views.html.emails.resetPassword(user, url).body)
              ))
              result
            }
          case None => Future.successful(result)
        }
      }
    }
  }
}
