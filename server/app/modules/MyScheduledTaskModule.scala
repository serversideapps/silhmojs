package modules

import net.codingwell.scalaguice.ScalaModule
import play.api.libs.concurrent.AkkaGuiceSupport

import javax.inject._
import akka.actor._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import com.typesafe.akka.extension.quartz.QuartzSchedulerExtension

class MyScheduledTask @Inject() (
  system: ActorSystem,
  @Named("update-scheduled-actor") scheduledActor: ActorRef
) {
  system.scheduler.schedule(
    initialDelay = 1.seconds,
    interval = 1.seconds,
    receiver = scheduledActor,
    message = controllers.TickEvery1Second
  )

  val scheduler = QuartzSchedulerExtension(system)
  scheduler.schedule("Every1Minute", scheduledActor, controllers.QuartzTickEvery1Minute)

  val scheduledActorRef = scheduledActor
}

class MyScheduledTaskModule extends ScalaModule with AkkaGuiceSupport {

  def configure() = {
    bindActor[controllers.MyScheduledActor]("update-scheduled-actor");
    bind[MyScheduledTask].asEagerSingleton()
  }
}
