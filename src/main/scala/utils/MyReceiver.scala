package utils

import org.jgroups.{Message, ReceiverAdapter}

trait MyReceiver extends ReceiverAdapter {

  def receiveMessage(message: Message): Unit

  override def receive(msg: Message): Unit = receiveMessage(msg)
}
