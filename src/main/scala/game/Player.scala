package game

import scala.collection.mutable.ArrayBuffer

sealed trait PlayerType
case object Human extends PlayerType
case object Computer extends PlayerType

class Player(val name: String, val playerType: PlayerType) {
  val characters = ArrayBuffer[Character]()
  
  def isAlive: Boolean = !characters.isEmpty
}
