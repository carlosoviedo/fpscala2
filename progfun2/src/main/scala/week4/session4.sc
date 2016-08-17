import week4.{Adventure, Socket, Socket2}

import scala.util.{Failure, Success}

val adventure = Adventure()
val coins = adventure.collectCoins()
val treasure = adventure.buyTreasure(coins)

val socket = Socket()
val packet = socket.readFromMemory()
val confirmation = socket.sendToEurope(packet)

val socket2 = Socket2()
val packet2 = socket2.readFromMemory()
val confirmation2 =
  packet2.onComplete {
    case Success(p) => socket.sendToEurope(p)
    case Failure(t) => ???
  }