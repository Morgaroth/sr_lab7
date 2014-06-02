package actors

import akka.actor.{ActorLogging, Actor}
import actors.TestActor.{Iwona, Ala}

object TestActor {

  case class Ala()

  case class Iwona()

}


class TestActor extends Actor with ActorLogging {
  override def receive: Receive = {
    case Ala =>
      log.info("received Ala")
      import context._
      become(running)
  }

  def running: Receive = {
    case Iwona =>
      log.info("received Iwona")

  }

  override def unhandled(message: Any): Unit = log.info(s"unhandled $message")
}
