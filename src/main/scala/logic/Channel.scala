package logic

import org.jgroups.{JChannel, Message, View, ReceiverAdapter}

class Channel extends ReceiverAdapter {

  override def viewAccepted(view: View) {
    println(view)
  }

  override def receive(msg: Message) {
    println(msg)
  }

  var channel: JChannel = _

  def close() {
    channel.close()
  }
}
