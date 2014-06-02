package utils

import pl.edu.agh.dsrg.sr.chat.protos.ChatAction

object Messages {

  case class ChannelsList(channels: Map[String, Set[String]] = null)

  case class Login()


}