import scala.io.StdIn.readLine


class NullWrapper[T](val any: T) {
  val isNull = any == null
  val isntNull = !isNull
}

implicit def wrapNull[T](any: T) = new NullWrapper[T](any)

object Runner {
  def main(args: Array[String]) {
    printHelp()
    val nick = {
      readNick match {
        case Some(s) => s
        case None => return
      }
    }
    println(s"<<< Witaj $nick")
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
    print("Hej\npodaj nick: ")
    val nick = readLine()
    if (nick.isntNull) {
      Some(nick)
    }
    None
  }
}