package game.core

import scala.collection.mutable.ArrayBuffer

sealed trait PlayerType
case object Human extends PlayerType
case object Computer extends PlayerType

abstract class Player(val name: String, val playerType: PlayerType) {
  val characters = ArrayBuffer[Character]()

  def isAlive: Boolean = !characters.isEmpty
}

class HumanPlayer(name: String) extends Player(name, Human)

class AIPlayer(name: String, val ai: PlayerAI) extends Player(name, Computer)
