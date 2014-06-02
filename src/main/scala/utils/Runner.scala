package utils

import scala.annotation.tailrec
import scala.io.StdIn.readLine
import java.util.logging.Logger
import scala.util.matching.Regex.Match
import actors.{Commands, ManagementActor}
import scala.Some


class NullWrapper[T](val any: T) {
  val isNull = any == null
  val isntNull = !isNull
}

object NullWrapper {
  implicit def wrapNull[T](any: T) = new NullWrapper[T](any)
}

object Runner {

  val log = Logger.getLogger("runner")

  def readCommand() = readLine()

  object commands {

    import actors.Commands._

    def printChannels() = ManagementActor.actor ! PrintChannels

    def joinCanal(channelName: String) = ManagementActor.actor ! JoinChannel(channelName)

    def createAndJoin(channel: String) = log.severe("not implemented")

    def sendMessage(channel: String, body: String) = ManagementActor.actor ! Commands.Send(channel, body)

    def login(nick: String) = ManagementActor.actor ! Nick(nick)

    def shutdown() = ManagementActor.actor ! Terminate

    def leaveChannel(channel: String) = ManagementActor.actor ! Commands.Leave(channel)
  }

  @tailrec
  def process(): Unit = {
    import utils.CommandExtractors._
    import commands._
    printPrompt()
    val readed = readCommand()
    readed match {
      case Channels() => printChannels()
      case Join(kanal) => joinCanal(kanal)
      case Leave(channel) => leaveChannel(channel)
      case End() =>
        shutdown()
        return
      case Send((ch, body)) => sendMessage(ch, body)
      case Create(channel) => createAndJoin(channel)
      case "" | null =>
      case _ =>
        log.info("nierozpoznano " + readed)
        printHelp()
    }
    process()
  }

  def main(args: Array[String]) {
    System.setProperty("java.net.preferIPv4Stack", "true")
    printHelp()
    val nick = {
      readNick match {
        case Some(s) => s
        case None => return
      }
    }
    println(s"<<< Witaj $nick")
    commands.login(nick)

    process()
  }

  def printPrompt() {
    print(">>> ")
  }

  def printHelp() {
    println(
      """lista komend:
        |    kanały -> lista kanałów i użytkowników
        |    dołącz $kanał -> dółącza do istniejącego kanału
        |    stwórz $kanał -> tworzy i dołącza do kanału
        |    s $kanał $wiadomosc -> wysyła wiadomość do kanału"""
        .stripMargin)
  }

  def readNick: Option[String] = {
    import NullWrapper._
    print("Hej\npodaj nick: ")
    val nick = readLine()
    if (nick.isntNull)
      Some(nick)
    else
      None
  }
}

object CommandExtractors {

  object Join {
    val joinRegex = """^\s*(dołącz|j)\s*(\S*)\s*$""".r
    val longAddrres = """^(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$""".r
    val shortAddr = """^(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$""".r

    def unapply(line: String) = {
      val list: List[Match] = (joinRegex findAllMatchIn line).toList
      list match {
        case joinRegex(_, c) :: Nil =>
          if (c.size > 4)
            (longAddrres findAllMatchIn c).toList match {
              case longAddrres(p1, p2, p3, p4) :: Nil => Some(s"$p1.$p2.$p3.$p4")
              case _ => None
            }
          else
            (shortAddr findAllMatchIn c).toList match {
              case shortAddr(p4) :: Nil => Some(s"230.0.0.$p4")
              case _ => None
            }
        case _ => None
      }
    }
  }

  object Leave {
    val leaveRegex = """^\s*(wyjdź|w)\s*(\S*)\s*$""".r

    def unapply(line: String) = {
      val list: List[Match] = (leaveRegex findAllMatchIn line).toList
      list match {
        case leaveRegex(_, c) :: Nil => Some(c)
        case _ => None
      }
    }
  }

  object End {
    val endRegex = """^\s*(koniec|zakończ|wyjdź|q)\s*$"""

    def unapply(line: String) = line.matches(endRegex)
  }

  object Channels {
    val channelsRegex = """^\s*(kanały|l)\s*$"""

    def unapply(line: String) = line.matches(channelsRegex)
  }

  object Create {
    val createRegex = """^\s*(stwórz|c)\s*(\S*)\s*$""".r

    def unapply(line: String) = {
      (createRegex findAllMatchIn line).toList match {
        case createRegex(_, c) :: Nil => Some(c)
        case _ => None
      }
    }

  }

  object Send {
    val sendRegex = """^\s*(wyslij|s)\s*(\S*)\s*([\S\s]*)$""".r

    def unapply(line: String): Option[(String, String)] = {
      (sendRegex findAllMatchIn line).toList match {
        case sendRegex(_, channel, message) :: Nil => Some((channel, message))
        case _ => None
      }
    }

  }

}