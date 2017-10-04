package controllers

import javax.inject.Inject

import com.mohiva.play.silhouette.api.{ LogoutEvent, Silhouette, HandlerResult }
import com.mohiva.play.silhouette.impl.providers.SocialProviderRegistry

import play.api.i18n.{ I18nSupport, MessagesApi }
import play.api.mvc.{ AnyContentAsEmpty, Controller, Request, WebSocket }
import play.api.libs.streams.ActorFlow

import akka.actor.{ Actor, ActorRef, ActorSystem, Props }
import akka.stream.Materializer

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ ExecutionContext, Future }

import utils.auth.DefaultEnv
import models.User

object MyWebSocketActorUser {
  def props(myscheduledactor: ActorRef, user: Option[User])(out: ActorRef) =
    Props(new MyWebSocketActorUser(myscheduledactor, user, out))
}

class MyWebSocketActorUser(myscheduledactor: ActorRef, user: Option[User], out: ActorRef) extends Actor {
  def receive = {
    case msg: String => {
      myscheduledactor ! SocketRequestMsg(msg, user, out)
    }
  }
}

object MyPingActor {
  def props(myscheduledactor: ActorRef)(out: ActorRef) = Props(new MyPingActor(myscheduledactor, out))
}

class MyPingActor(myscheduledactor: ActorRef, out: ActorRef) extends Actor {
  def receive = {
    case msg: String => {
      try { Thread.sleep(20 + new scala.util.Random().nextInt(100)) } catch { case e: Throwable => {} }
      myscheduledactor ! SocketRequestMsg(msg, None, out)
      out ! msg
    }
  }
}

class SocketController @Inject() (
  val messagesApi: MessagesApi,
  silhouette: Silhouette[DefaultEnv],
  socialProviderRegistry: SocialProviderRegistry,
  userDAO: models.daos.UserDAO,
  settingDAO: models.daos.SettingDAO,
  myscheduledtask: modules.MyScheduledTask
)(implicit
  system: ActorSystem,
  materializer: Materializer,
  ec: ExecutionContext)
  extends Controller with I18nSupport {

  def socket = WebSocket.acceptOrResult[String, String] { request =>
    implicit val req = Request(request, AnyContentAsEmpty)
    //println("socket request: " + req)
    silhouette.UserAwareRequestHandler { userAwareRequest =>
      Future.successful(HandlerResult(Ok, userAwareRequest.identity))
    }.map {
      case HandlerResult(r, Some(user)) => {
        //println("socket logged in [ " + user.name.getOrElse("user") + " ]");
        Right(ActorFlow.actorRef(MyWebSocketActorUser.props(myscheduledtask.scheduledActorRef, Some(user))))
      }
      case HandlerResult(r, None) => {
        //println("socket anon");
        Right(ActorFlow.actorRef(MyWebSocketActorUser.props(myscheduledtask.scheduledActorRef, None)))
      }
    }
  }

  def ping = WebSocket.acceptOrResult[String, String] { request =>
    implicit val req = Request(request, AnyContentAsEmpty)
    silhouette.UserAwareRequestHandler { userAwareRequest =>
      Future.successful(HandlerResult(Ok, userAwareRequest.identity))
    }.map {
      case HandlerResult(r, Some(user)) => {
        Right(ActorFlow.actorRef(MyPingActor.props(myscheduledtask.scheduledActorRef)))
      }
      case HandlerResult(r, None) => {
        //println("r " + r)
        Right(ActorFlow.actorRef(MyPingActor.props(myscheduledtask.scheduledActorRef)))
      }
    }
  }

}