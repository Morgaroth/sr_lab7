package actors

import com.google.protobuf.MessageLite
import org.jgroups.Message

object Helpers {
  implicit def implicitChatMessage2jchannelMessage[T <: MessageLite](c: T) = new Message(null, null, c.toByteArray)

}
