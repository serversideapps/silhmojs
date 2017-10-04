package controllers

import javax.inject.Inject

import com.mohiva.play.silhouette.api.{ LogoutEvent, Silhouette }
import com.mohiva.play.silhouette.impl.providers.SocialProviderRegistry

import play.api.i18n.{ I18nSupport, Messages, MessagesApi }
import play.api.mvc.Controller

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import java.util.UUID

import utils.auth.DefaultEnv
import forms.SettingsForm

import shared._

import upickle.default._

object Translations {
  val Chess = List(
    "ccfg.connecting", "ccfg.createtable", "ccfg.creatingtable", "ccfg.flip", "ccfg.play", "ccfg.playai",
    "ccfg.stand", "ccfg.resign", "ccfg.refresh", "ccfg.load", "ccfg.created", "ccfg.status",
    "ccfg.open", "ccfg.inprogress", "ccfg.terminated", "ccfg.result",
    "ccfg.white", "ccfg.black", "ccfg.yellow", "ccfg.red", "ccfg.variant", "ccfg.timecontrol"
  ) ::: PgnTranslations.RESULTREASONS
  val Analysis = List(
    "ccfg.book", "ccfg.notes", "ccfg.presentation", "ccfg.flip", "ccfg.yes", "ccfg.no", "ccfg.current",
    "ccfg.id", "ccfg.gennew", "ccfg.title", "ccfg.upload", "ccfg.owner", "ccfg.importpgn", "ccfg.savenotes",
    "ccfg.version", "ccfg.nodelink", "ccfg.nodeurl", "ccfg.create",
    "ccfg.savemovenote", "ccfg.editmovenote", "ccfg.essay", "ccfg.saveessay",
    "presentation.archive", "presentation.archived", "presentation.notarchived", "presentation.changearchived",
    "presentation.hybernate", "presentation.hybernated", "presentation.nothybernated", "presentation.changehybernated",
    "presentation.engine",
    "ccfg.change"
  )
}

class ApplicationController @Inject() (
  val messagesApi: MessagesApi,
  silhouette: Silhouette[DefaultEnv],
  socialProviderRegistry: SocialProviderRegistry,
  userDAO: models.daos.UserDAO,
  settingDAO: models.daos.SettingDAO,
  viewDAO: models.daos.ViewDAO,
  gameDAO: models.daos.GameDAO
)
  extends Controller with I18nSupport {

  def index = silhouette.SecuredAction.async { implicit request =>
    Future.successful(Ok(views.html.home(request.identity)))
  }

  def games = silhouette.SecuredAction.async { implicit request =>
    val handle = request.identity.handle.getOrElse("")
    gameDAO.find(handle).flatMap {
      gamelist => Future.successful(Ok(views.html.games(request.identity, gamelist.reverse)))
    }
  }

  def presentations = silhouette.SecuredAction.async { implicit request =>
    val handle = request.identity.handle.getOrElse("")
    gameDAO.findpres(handle).flatMap {
      gamelist => Future.successful(Ok(views.html.presentations(request.identity, gamelist.reverse)))
    }
  }

  def delpresentation(presid: String) = silhouette.SecuredAction.async { implicit request =>
    val handle = request.identity.handle.getOrElse("")
    gameDAO.get(presid).flatMap { optg =>
      optg match {
        case Some(g) => {
          if (g.presentation.owner == handle) {
            gameDAO.delete(presid).flatMap { delresult =>
              Future.successful(Redirect(routes.ApplicationController.presentations).flashing("success" -> Messages("presentation.deleted")))
            }
          } else Future.successful(Redirect(routes.ApplicationController.presentations).flashing("error" -> Messages("presentation.access.denied")))
        }
        case None => {
          Future.successful(Redirect(routes.ApplicationController.presentations).flashing("error" -> Messages("presentation.doesnotexist")))
        }
      }
    }
  }

  def settings = silhouette.SecuredAction.async { implicit request =>
    settingDAO.get(request.identity.userID).flatMap { setting =>
      Future.successful(Ok(views.html.settings(
        request.identity, setting.getOrElse(shared.Setting())
      )))
    }
  }

  def submitsettings = silhouette.SecuredAction.async { implicit request =>
    SettingsForm.form.bindFromRequest.fold(
      form => Future.successful(Redirect(routes.ApplicationController.settings).flashing("error" -> Messages("there.were.errors"))),
      data =>
        {
          val setting = shared.Setting(
            piecetype = Some(data.piecetype),
            note = Some(data.note)
          )
          settingDAO.set(request.identity.userID, setting).flatMap { setting =>
            Future.successful(Redirect(routes.ApplicationController.settings).flashing("success" -> Messages("changes.saved")))
          }

        }
    )
  }

  def chess = silhouette.UserAwareAction.async { implicit request =>
    val ccfg = write(ChessConfig(
      kind = "play",
      translations = (for (phrase <- Translations.Chess) yield (phrase -> Messages(phrase))).toMap
    ))
    request.identity match {
      case Some(identity) => settingDAO.get(identity.userID).flatMap { setting =>
        {
          viewDAO.get(identity.userID.toString, "chess").flatMap {
            viewserialized =>
              viewserialized match {
                case Some(content) => Future.successful(Ok(views.html.chess(Some(identity), setting, content.content, chessconfig = ccfg)))
                case _ => Future.successful(Ok(views.html.chess(Some(identity), setting, chessconfig = ccfg)))
              }

          }
        }
      }
      case _ => Future.successful(Ok(views.html.chess(None, None, chessconfig = ccfg)))
    }
  }

  def analysis(gameIDstr: String = "none") = silhouette.UserAwareAction.async { implicit request =>
    var gameID = gameIDstr
    var nodeid = -1

    if (gameIDstr.contains("--")) {
      val giparts = gameIDstr.split("\\-\\-").toList
      gameID = giparts(0)
      nodeid = giparts(1).toInt
    }

    val chessconfig = ChessConfig(
      kind = "analysis",
      gameID = gameID,
      currentnodeid = nodeid,
      translations = (for (phrase <- Translations.Analysis) yield (phrase -> Messages(phrase))).toMap
    )

    var alttitle: Option[String] = Some(Messages("analysis.deftitle"))

    def viewbyidentity = {
      request.identity match {
        case Some(identity) => settingDAO.get(identity.userID).flatMap { setting =>
          {
            viewDAO.get(identity.userID.toString, "analysis").flatMap {
              viewserialized =>
                viewserialized match {
                  case Some(content) => Future.successful(Ok(views.html.chess(Some(identity), setting, content.content, chessconfig = write(chessconfig), alttitle = alttitle)))
                  case _ => Future.successful(Ok(views.html.chess(Some(identity), setting, chessconfig = write(chessconfig), alttitle = alttitle)))
                }
            }
          }
        }
        case _ => Future.successful(Ok(views.html.chess(None, None, chessconfig = write(chessconfig), alttitle = alttitle)))
      }
    }
    if (gameID == "none") {
      val deftitle = Messages("presentation.deftitle")
      chessconfig.presgame.presentationtitle = deftitle
      chessconfig.presgame.presentation.title = deftitle
      viewbyidentity
    } else {
      gameDAO.get(gameID).flatMap { optg =>
        optg match {
          case Some(g) => {
            alttitle = Some(g.presentation.title)
            if (g.presentation.title == "") {
              alttitle = Some(Messages("presentation.deftitle"))
              g.presentationtitle = alttitle.get
              g.presentation.title = alttitle.get
            }
            if (g.presentationid == "") {
              val testg = new smartchess.game(g.variant)
              testg.parse_pgn(g.pgn, head_only = true)
              alttitle = Some(Messages("chessappgame.title") + " " +
                testg.get_header("White") + " - " +
                testg.get_header("Black") + " " +
                testg.get_header("Result")
              )
            }
            chessconfig.presgame = g
            viewbyidentity
          }
          case None => {
            viewbyidentity
          }
        }
      }
    }
  }

  def dev = silhouette.SecuredAction.async { implicit request =>
    if (request.identity.admin) userDAO.findall().flatMap { userlist =>
      Future.successful(Ok(views.html.dev(Some(request.identity), userlist.sortWith((a, b) => a.userID.compareTo(b.userID) > 0))))
    }
    else Future.successful(Redirect(routes.ApplicationController.index()))
  }

  def devflip(userID: String) = silhouette.SecuredAction.async { implicit request =>
    val uuid = java.util.UUID.fromString(userID)
    if (request.identity.admin) userDAO.find(uuid).flatMap {
      user =>
        user match {
          case Some(u) => userDAO.save(u.setActivated(!u.activated)).flatMap { user =>
            try { Thread.sleep(100) } catch { case e: Throwable => }
            userDAO.findall().flatMap { userlist =>
              Future.successful(Ok(views.html.dev(Some(request.identity), userlist.sortWith((a, b) => a.userID.compareTo(b.userID) > 0))))
            }
          }
          case None => userDAO.findall().flatMap { userlist =>
            Future.successful(Ok(views.html.dev(Some(request.identity), userlist.sortWith((a, b) => a.userID.compareTo(b.userID) > 0))))
          }
        }
    }
    else Future.successful(Redirect(routes.ApplicationController.index()))
  }

  def signOut = silhouette.SecuredAction.async { implicit request =>
    val result = Redirect(routes.ApplicationController.index())
    silhouette.env.eventBus.publish(LogoutEvent(request.identity, request))
    silhouette.env.authenticatorService.discard(request.authenticator, result)
  }
}
