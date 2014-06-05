package actors

import utils.MyReceiver
import akka.actor._
import pl.edu.agh.dsrg.sr.chat.protos.{ChatState, ChatAction}
import actors.ManagementActor._
import org.jgroups.{Message, JChannel, ReceiverAdapter, View}
import Helpers.implicitChatMessage2jchannelMessage
import ChatAction.ActionType.{JOIN, LEAVE}
import java.io.{OutputStream, InputStream}
import org.jgroups.stack.ProtocolStack
import org.jgroups.protocols._
import org.jgroups.protocols.pbcast._
import actors.Commands._
import scala.Some
import utils.Messages.ChannelsList
import actors.Commands.SetChannels
import akka.actor.Terminated
import actors.Commands.Logout
import java.net.InetAddress

object ManagementActor {
  private lazy val system = ActorSystem("chat")
  lazy val actor = system.actorOf(Props[ManagementActor])

  def getChannel(name: String, listener: ReceiverAdapter, address: String = null,management: Boolean=false) = {
    val channel = new JChannel(false)
    val udp = address match {
      case null => new UDP()
      case _ => new UDP().setValue("mcast_group_addr", InetAddress.getByName(address))
    }
    val stack = new ProtocolStack()
    channel.setProtocolStack(stack)
    stack.addProtocol(udp)
      .addProtocol(new PING())
      .addProtocol(new MERGE2())
      .addProtocol(new FD_SOCK())
      .addProtocol(new FD_ALL().setValue("timeout", 12000).setValue("interval", 3000))
      .addProtocol(new VERIFY_SUSPECT())
      .addProtocol(new BARRIER())
      .addProtocol(new NAKACK())
      .addProtocol(new UNICAST2())
      .addProtocol(new STABLE())
      .addProtocol(new GMS())
      .addProtocol(new UFC())
      .addProtocol(new MFC())
      .addProtocol(new FRAG2())
      .addProtocol(new STATE_TRANSFER())
      .addProtocol(new FLUSH())
    stack.init()
    channel.setReceiver(listener)
    channel.setName(name)
    if(management){
      channel.connect("ChatManagement768264")
      }else{
      channel.connect(address)  
      }
    channel
  }

}


class ManagementActor extends MyReceiver with Actor with ActorLogging {

  val managementChannel = {
    try {
      val channel = getChannel("mateusz", this,null,true)
      channel.getState(null, 10000)
      channel
    } catch {
      case e: Throwable => e.printStackTrace()
        null
    }

  }
  var channels = Map[String, Set[ChatAction]]()
  var privChannels = Map[String, ActorRef]()
  var nick: String = "nick-uncompleted"

  def receive: Receive = {
    case chatMsg: ChatAction =>
      chatMsg.`action` match {
        case JOIN =>
          log.info(s"${chatMsg.`nickname`} join at ${chatMsg.`channel`}")
          channels =
            channels.get(chatMsg.`channel`) match {
              case None => channels + (chatMsg.`channel` -> Set[ChatAction](chatMsg))
              case Some(set) => channels + (chatMsg.`channel` -> (set + chatMsg))
            }
        case LEAVE =>
          log.info(s"${chatMsg.`nickname`} leavr from ${chatMsg.`channel`}")
          channels = channels.get(chatMsg.`channel`) match {
            case None => channels
            case Some(set) => channels + (chatMsg.`channel` -> set.filter(p => p.`channel` != chatMsg.`channel` || p.`nickname` != chatMsg.`nickname`))
          }
      }

    case SetChannels(ch) =>
      channels = ch
      //log.info("state setted " + channels.mapValues(_.mkString("{", ",", "}")).mkString("[", ",", "]"))

    case v: View =>

    case JoinChannel(channel) =>
      log.info(s"user joins to channel $channel")
      privChannels.get(channel) match {
        case None =>
          val actor = system.actorOf(Props[ChannelActor], channel)
          privChannels += channel -> actor
          actor ! ChannelInit(channel, nick)
          context.watch(actor)
          val msg = ChatAction(ChatAction.ActionType.JOIN, channel, nick)
          managementChannel.send(msg)
        case Some(_) =>
          println("kanał już otwarty !")
      }

    case Logout(channel) =>

    case ChannelsList(_) =>
      sender ! ChannelsList(channels.map(x => x._1 -> x._2.map(_.`nickname`).toSet).toMap)

    case Nick(n) =>
      nick = n

    case Send(channel, body) =>
      privChannels.get(channel) match {
        case Some(ac) =>
          ac ! ChannelActor.Send(body)
        case None => log.error("kanał nie istnieje")
      }

    case Leave(channel) =>
      privChannels = privChannels.get(channel) match {
        case Some(actorRef) =>
          actorRef ! Terminate
          managementChannel.send(ChatAction(LEAVE, channel, nick))
          privChannels - channel
        case None =>
          log.error("kanał nie istnieje")
          privChannels
      }

    case Terminate =>
      log.info("manager received terminate signal")
      val msg = ChatAction(LEAVE, null, nick)
      privChannels.foreach {
        case (channel, actorRef) =>
          actorRef ! Terminate
          managementChannel.send(msg.copy(`channel` = channel))
      }
      context.become(shuttingDown)

    case PrintChannels =>
      println(channels.map(x => x._1 -> x._2.map(_.`nickname`).toSet.mkString("{", ",", "}")).toMap.mkString("[", ",", "]"))

    case Terminated(actorRef) =>
      log.info(s"actor $actorRef terminate")
      privChannels = privChannels.filterNot {
        case (_, ac) => ac.equals(actorRef)
      }

  }

  def shuttingDown: Receive = {

    case Terminated(actorRef) =>
      log.info(s"actor $actorRef terminate")
      privChannels = privChannels.filterNot {
        case (_, ac) => ac.equals(actorRef)
      }
      if (privChannels.isEmpty) {
        context.stop(self)
        system.shutdown()
      }

    case a => log.error("actor is shutting " + a)
  }

  @scala.throws[Exception](classOf[Exception])
  override def postStop() {
    log.info("management actor stops")
    managementChannel.close()
  }

  def receiveMessage(msg: Message) {
    val parsed = ChatAction.getDefaultInstance.mergeFrom(msg.getBuffer)
    self ! parsed
  }

  override def viewAccepted(view: View) {
    Option(view) match {
      case Some(v) => self ! v
      case _ =>
    }
  }

  override def getState(output: OutputStream) {
    val tmp = channels
    output.write(ChatState(tmp.values.flatten.toVector).toByteArray)
  }

  override def setState(input: InputStream) {
    val ChatState(list) = ChatState.getDefaultInstance.mergeFrom(input)
    self ! SetChannels {
      list.map(t => t.`channel` -> t).groupBy(_._1).map {
        case (ch, users) => ch -> users.map(_._2).toSet
      }
    }
  }
}


object Commands {

  case class ChannelInit(channelName: String, nick: String)

  case class JoinChannel(channelName: String)

  case class Nick(nick: String)

  case class LogoutAll()

  case class Logout(channel: String)

  case class SetChannels(channels: Map[String, Set[ChatAction]])

  case class Send(channel: String, message: String)

  case class Terminate()

  case class PrintChannels()

  case class Leave(channel: String)

}
