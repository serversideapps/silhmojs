package controllers

import javax.inject.Inject

import com.mohiva.play.silhouette.api.Authenticator.Implicits._
import com.mohiva.play.silhouette.api._
import com.mohiva.play.silhouette.api.exceptions.ProviderException
import com.mohiva.play.silhouette.api.repositories.AuthInfoRepository
import com.mohiva.play.silhouette.api.util.{ Clock, Credentials }
import com.mohiva.play.silhouette.impl.exceptions.IdentityNotFoundException
import com.mohiva.play.silhouette.impl.providers._
import models.services.{ UserService }
import net.ceedubs.ficus.Ficus._
import play.api.Configuration
import play.api.i18n.{ I18nSupport, Messages, MessagesApi }
import play.api.libs.concurrent.Execution.Implicits._
import play.api.mvc.Controller
import utils.auth.DefaultEnv

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.language.postfixOps

class SignInController @Inject() (
  val messagesApi: MessagesApi,
  silhouette: Silhouette[DefaultEnv],
  userService: UserService,
  authInfoRepository: AuthInfoRepository,
  credentialsProvider: CredentialsProvider,
  socialProviderRegistry: SocialProviderRegistry,
  configuration: Configuration,
  clock: Clock,
  envDAO: models.daos.EnvDAO
)
  extends Controller with I18nSupport {

  def view = silhouette.UnsecuredAction.async { implicit request =>
    Future.successful(Ok(views.html.signIn(
      new myform.MySignInForm(envDAO.getDosendmail), socialProviderRegistry, envDAO.getDosendmail
    )))
  }

  def submit = silhouette.UnsecuredAction.async { implicit request =>
    new myform.MySignInForm(envDAO.getDosendmail).bindFromRequest match {
      case form: myform.MySignInForm =>
        Future.successful(BadRequest(views.html.signIn(form, socialProviderRegistry, envDAO.getDosendmail)))
      case data: myform.MySignInFormData => {
        val credentials = Credentials(data.email, data.password)
        credentialsProvider.authenticate(credentials).flatMap { loginInfo =>
          val result = Redirect(routes.ApplicationController.index())
          userService.retrieve(loginInfo).flatMap {
            case Some(user) if !user.activated =>
              Future.successful(Ok(views.html.activateAccount(data.email)))
            case Some(user) =>
              val c = configuration.underlying
              silhouette.env.authenticatorService.create(loginInfo).map {
                case authenticator if data.rememberMe =>
                  authenticator.copy(
                    expirationDateTime = clock.now + c.as[FiniteDuration]("silhouette.authenticator.rememberMe.authenticatorExpiry"),
                    idleTimeout = c.getAs[FiniteDuration]("silhouette.authenticator.rememberMe.authenticatorIdleTimeout"),
                    cookieMaxAge = c.getAs[FiniteDuration]("silhouette.authenticator.rememberMe.cookieMaxAge")
                  )
                case authenticator => authenticator
              }.flatMap { authenticator =>
                silhouette.env.eventBus.publish(LoginEvent(user, request))
                silhouette.env.authenticatorService.init(authenticator).flatMap { v =>
                  silhouette.env.authenticatorService.embed(v, result)
                }
              }
            case None => { println("user not found"); Future.failed(new IdentityNotFoundException("Couldn't find user")) }
          }
        }.recover {
          case e: ProviderException =>
            Redirect(routes.SignInController.view()).flashing("error" -> Messages("invalid.credentials"))
        }
      }
    }
  }
}
