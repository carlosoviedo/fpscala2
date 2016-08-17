package week4

import scala.concurrent.Future

trait Adventure {
  type Coin = Int
  type Treasure = Any
  def collectCoins(): List[Coin] = ???
  def buyTreasure(coins: List[Coin]): Treasure = ???
}

object Adventure {
  def apply() = new Adventure{}
}

trait Socket {
  def readFromMemory(): Array[Byte] = ???
  def sendToEurope(packet: Array[Byte]): Array[Byte] = ???
}

object Socket {
  def apply() = new Socket{}
}

trait Socket2 {
  def readFromMemory(): Future[Array[Byte]] = ???
  def sendToEurope(packet: Array[Byte]): Future[Array[Byte]] = ???
}

object Socket2 {
  def apply() =  new Socket2{}
}