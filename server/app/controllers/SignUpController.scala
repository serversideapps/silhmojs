package controllers

import java.util.UUID
import javax.inject.Inject
import play.api._
import com.mohiva.play.silhouette.api._
import com.mohiva.play.silhouette.api.repositories.AuthInfoRepository
import com.mohiva.play.silhouette.api.services.AvatarService
import com.mohiva.play.silhouette.api.util.PasswordHasherRegistry
import com.mohiva.play.silhouette.impl.providers._
import models.User
import models.services.{ AuthTokenService, UserService }
import models.daos._
import shared.Setting
import play.api.i18n.{ I18nSupport, Messages, MessagesApi }
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.mailer.{ Email, MailerClient }
import play.api.mvc.Controller
import utils.auth.DefaultEnv
import scala.concurrent.Future
import play.api.libs.ws.WSClient

class SignUpController @Inject() (
  val messagesApi: MessagesApi,
  silhouette: Silhouette[DefaultEnv],
  userService: UserService,
  envDAO: EnvDAO,
  settingDAO: SettingDAO,
  authInfoRepository: AuthInfoRepository,
  authTokenService: AuthTokenService,
  avatarService: AvatarService,
  passwordHasherRegistry: PasswordHasherRegistry,
  mailerClient: MailerClient,
  userDAO: UserDAO,
  wsClient: WSClient,
  implicit val configuration: play.api.Configuration
)
  extends Controller with I18nSupport {

  def view = silhouette.UnsecuredAction.async { implicit request =>
    Future.successful(Ok(views.html.signUp(new myform.MySignUpForm(envDAO.getDosendmail), envDAO.getDocaptcha, envDAO.getDosendmail)))
  }

  def submit = silhouette.UnsecuredAction.async { implicit request =>
    val handledata: (myform.MySignUpFormData) => scala.concurrent.Future[play.api.mvc.Result] = {
      data =>
        userDAO.find(data.handle).flatMap { user =>
          user match {
            case Some(user) => {
              val form = new myform.MySignUpForm(envDAO.getDosendmail)
              form.d = data
              form.errors += ("handle" -> "Handle already exists.")
              Future.successful(Ok(views.html.signUp(form, envDAO.getDocaptcha, envDAO.getDosendmail)))
            }
            case None =>
              {
                val result = Redirect(routes.SignInController.view()).flashing(
                  if (envDAO.getDosendmail) "info" -> Messages("sign.up.email.sent", data.email)
                  else "success" -> """You can now sign in."""
                )
                val loginInfo = LoginInfo(CredentialsProvider.ID, data.email)
                userService.retrieve(loginInfo).flatMap {
                  case Some(user) =>
                    val url = routes.SignInController.view().absoluteURL()
                    println("url " + url)
                    if (envDAO.getDosendmail) mailerClient.send(Email(
                      subject = Messages("email.already.signed.up.subject"),
                      from = Messages("email.from"),
                      to = Seq(data.email),
                      bodyText = Some(views.txt.emails.alreadySignedUp(user, url).body),
                      bodyHtml = Some(views.html.emails.alreadySignedUp(user, url).body)
                    ))

                    Future.successful(result)
                  case None =>
                    val authInfo = passwordHasherRegistry.current.hash(data.password)
                    val user = User(
                      userID = UUID.randomUUID(),
                      loginInfo = loginInfo,
                      handle = Some(data.handle),
                      regpass = None,
                      firstName = Some(data.firstName),
                      lastName = Some(data.lastName),
                      fullName = Some(data.firstName + " " + data.lastName),
                      email = Some(data.email),
                      avatarURL = None,
                      activated = !envDAO.getDosendmail
                    )
                    for {
                      avatar <- avatarService.retrieveURL(data.email)
                      user <- userService.save(user.copy(avatarURL = avatar))
                      authInfo <- authInfoRepository.add(loginInfo, authInfo)
                      authToken <- authTokenService.create(user.userID)
                    } yield {
                      val url = routes.ActivateAccountController.activate(authToken.id).absoluteURL()
                      println("url " + url)
                      if (envDAO.getDosendmail) mailerClient.send(Email(
                        subject = Messages("email.sign.up.subject"),
                        from = Messages("email.from"),
                        to = Seq(data.email),
                        bodyText = Some(views.txt.emails.signUp(user, url).body),
                        bodyHtml = Some(views.html.emails.signUp(user, url).body)
                      ))

                      silhouette.env.eventBus.publish(SignUpEvent(user, request))
                      result
                    }
                }
              }
          }
        }
    }

    new myform.MySignUpForm(envDAO.getDosendmail).bindFromRequestAndVerify(envDAO.getDocaptcha).flatMap { result =>
      result match {
        case form: myform.MySignUpForm => {
          println("signup failed " + form.errors);
          Future.successful(BadRequest(views.html.signUp(form, envDAO.getDocaptcha, envDAO.getDosendmail)))
        }
        case data: myform.MySignUpFormData => { println("recaptcha ok"); handledata(data) }
      }
    }
  }
}
