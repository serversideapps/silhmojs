package controllers

import javax.inject.Inject

import com.mohiva.play.silhouette.api._
import com.mohiva.play.silhouette.api.exceptions.ProviderException
import com.mohiva.play.silhouette.api.repositories.AuthInfoRepository
import com.mohiva.play.silhouette.impl.providers._
import models.services.UserService
import play.api.i18n.{ I18nSupport, Messages, MessagesApi }
import play.api.libs.concurrent.Execution.Implicits._
import play.api.mvc.{ Action, Controller }
import utils.auth.DefaultEnv

import scala.concurrent.Future

class SocialAuthController @Inject() (
  val messagesApi: MessagesApi,
  silhouette: Silhouette[DefaultEnv],
  userService: UserService,
  authInfoRepository: AuthInfoRepository,
  socialProviderRegistry: SocialProviderRegistry,
  userDAO: models.daos.UserDAO
)
  extends Controller with I18nSupport with Logger {

  /*def authenticate(provider: String) = Action.async { implicit request =>
    (socialProviderRegistry.get[SocialProvider](provider) match {
      case Some(p: SocialProvider with CommonSocialProfileBuilder) =>
        p.authenticate().flatMap {
          case Left(result) => Future.successful(result)
          case Right(authInfo) => for {
            profile <- { println("authinfo " + authInfo); p.retrieveProfile(authInfo) }
            user <- userService.save(profile)
            authInfo <- authInfoRepository.save(profile.loginInfo, authInfo)
            authenticator <- silhouette.env.authenticatorService.create(profile.loginInfo)
            value <- silhouette.env.authenticatorService.init(authenticator)
            result <- silhouette.env.authenticatorService.embed(value, Redirect(routes.ApplicationController.index()))
          } yield {
            silhouette.env.eventBus.publish(LoginEvent(user, request))
            result
          }
        }
      case _ => Future.failed(new ProviderException(s"Cannot authenticate with unexpected social provider $provider"))
    }).recover {
      case e: ProviderException =>
        logger.error("Unexpected provider error", e)
        Redirect(routes.SignInController.view()).flashing("error" -> Messages("could.not.authenticate"))
    }
  }*/

  def submithandle(provider: String, token: java.util.UUID) = Action.async { implicit request =>
    new myform.MyGetHandleForm().bindFromRequest match {
      case form: myform.MyGetHandleForm => Future.successful(BadRequest(views.html.getHandle(form, provider, token)))
      case data: myform.MyGetHandleFormData => userDAO.find(data.handle).flatMap { user =>
        user match {
          case Some(user) => {
            val form = new myform.MyGetHandleForm()
            form.d = data
            form.errors += ("handle" -> "Handle already exists.")
            Future.successful(BadRequest(views.html.getHandle(form, provider, token)))
          }
          case None =>
            {
              val p = socialProviders(token)
              val authInfo = authInfos(token).asInstanceOf[p.A]
              println("Authenticating user with handle " + data.handle)
              for {
                profile <- p.retrieveProfile(authInfo)
                user <- userService.save(profile, data.handle)
                authInfo <- authInfoRepository.save(profile.loginInfo, authInfo)
                authenticator <- silhouette.env.authenticatorService.create(profile.loginInfo)
                value <- silhouette.env.authenticatorService.init(authenticator)
                result <- silhouette.env.authenticatorService.embed(value, Redirect(routes.ApplicationController.index()))
              } yield {
                silhouette.env.eventBus.publish(LoginEvent(user, request))
                result
              }
            }
        }
      }
    }
  }

  var authInfos = Map[java.util.UUID, Any]()
  var socialProviders = Map[java.util.UUID, SocialProvider with CommonSocialProfileBuilder]()

  def authenticate(provider: String) = Action.async { implicit request =>
    (socialProviderRegistry.get[SocialProvider](provider) match {
      case Some(p: SocialProvider with CommonSocialProfileBuilder) =>
        println("Authenticate handle")
        p.authenticate().flatMap {
          case Left(result) => { println("result " + result); Future.successful(result) }
          case Right(authInfo) => {
            p.retrieveProfile(authInfo).flatMap { profile =>
              userDAO.find(profile.loginInfo).flatMap { user =>
                user match {
                  case Some(user) => {
                    val handle = user.handle.get
                    println("User already exists, handle: " + handle)
                    for {
                      profile <- p.retrieveProfile(authInfo)
                      user <- userService.save(profile, handle)
                      authInfo <- authInfoRepository.save(profile.loginInfo, authInfo)
                      authenticator <- silhouette.env.authenticatorService.create(profile.loginInfo)
                      value <- silhouette.env.authenticatorService.init(authenticator)
                      result <- silhouette.env.authenticatorService.embed(value, Redirect(routes.ApplicationController.index()))
                    } yield {
                      silhouette.env.eventBus.publish(LoginEvent(user, request))
                      result
                    }
                  }
                  case None => {
                    println("User does not exist, obtaining handle")
                    val token = java.util.UUID.randomUUID()
                    authInfos += (token -> authInfo)
                    socialProviders += (token -> p)
                    Future.successful(Ok(views.html.getHandle(new myform.MyGetHandleForm(), provider, token)))
                  }
                }
              }
            }
          }
        }
      case _ => Future.failed(new ProviderException(s"Cannot authenticate with unexpected social provider $provider"))
    }).recover {
      case e: ProviderException =>
        logger.error("Unexpected provider error", e)
        Redirect(routes.SignInController.view()).flashing("error" -> Messages("could.not.authenticate"))
    }
  }
}
