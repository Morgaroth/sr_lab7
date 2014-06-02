package actors

import akka.actor.{ActorRef, Actor, ActorLogging}
import org.jgroups.{JChannel, View, Message}
import utils.MyReceiver
import pl.edu.agh.dsrg.sr.chat.protos.ChatMessage
import actors.Commands.{Terminate, Send, ChannelInit}
import Helpers.implicitChatMessage2jchannelMessage

object ChannelActor {

  case class Ok()

  case class Send(msg: String)

}

class ChannelActor extends MyReceiver with Actor with ActorLogging {

  var channel: JChannel = _
  var watcher: ActorRef = _
  var nick: String = _

  override def receive: Receive = {
    case v: View => log.info("received view " + v)

    case msg: ChatMessage =>
      println(s"\n-----------------------------\nRECEIVED: ${msg.`message`}\n-----------------------")

    case ChannelActor.Send(msg) =>
      channel.send(ChatMessage(`message` = msg))

    case ChannelInit(channelName, nickname) =>
      channel = ManagementActor.getChannel(nickname, this,channelName)
      nick = nickname
      log.info(s"actor for channel $channelName started")

    case Terminate =>
      log.info(s"actor for $channel gets Terminate sugnal")
      context.stop(self)
  }

  @scala.throws[Exception](classOf[Exception])
  override def postStop() {
    log.info(s"actor for channel ${channel.getName} killed")
    channel.close()
  }

  def receiveMessage(msg: Message) {
    val message = ChatMessage.getDefaultInstance.mergeFrom(msg.getBuffer)
    self ! message
  }

  override def viewAccepted(view: View) {
    Option(view) match {
      case Some(v) => self ! v
      case _ =>
    }
  }

  override def unhandled(message: Any): Unit = {
    log.error(s"unhandled $message")
  }
}
